%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014-2022 Mikael Pettersson
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%
%%% es_eval.erl
%%%
%%% An R7RS Core Forms Evaluator for ErlScheme.
%%%
%%% Notes:
%%% - LETREC is implemented via finite unfolding of a local
%%%   non-recursive environment fragment
%%% - Evaluation is a two-step process: a top-level S-expression is
%%%   first parsed and converted to an abstract syntax tree (AST),
%%%   which is then interpreted.  Function closures record their
%%%   bodies as ASTs, not as S-expressions.
%%% - Variadic functions (with "rest" parameters) are not supported, since
%%%   they mess up calling conventions and interoperability with Erlang.
%%%
%%% Extensions:
%%% - (: M F A) is equivalent to Erlang's fun M:F/A
%%% - (case ...) performs Erlang-like pattern-matching with optional guards,
%%%   Scheme's (case ...) is not supported

-module(es_eval).

-export([ eval/2
        ]).

-type sexpr() :: term().
-type datum() :: term().

%% API -------------------------------------------------------------------------

-spec eval(sexpr(), es_macros:synenv()) -> {datum(), es_macros:synenv()}.
eval(Sexpr, SynEnv) ->
  %% io:format("before expand:\n~p\n", [Sexpr]),
  {Expanded, NewSynEnv} = es_macros:expand_toplevel(Sexpr, SynEnv),
  %% io:format("after expand:\n~p\n", [Expanded]),
  AST = es_parse:toplevel(Expanded),
  %% io:format("after parse:\n~p\n", [AST]),
  Datum = interpret(AST, es_env:empty()),
  {Datum, NewSynEnv}.

%% Internals (AST interpreter) -------------------------------------------------

interpret(AST, Env) ->
  case AST of
    {'ES:BEGIN', First, Next} ->
      interpret_begin(First, Next, Env);
    {'ES:CASE', Expr, Clauses} ->
      interpret_case(Expr, Clauses, Env);
    {'ES:CONS', Hd, Tl} ->
      interpret_cons(Hd, Tl, Env);
    {'ES:DEFINE', Var, Expr} ->
      interpret_define(Var, Expr, Env);
    {'ES:GLOVAR', Var} ->
      interpret_glovar(Var);
    {'ES:IF', Pred, Then, Else} ->
      interpret_if(Pred, Then, Else, Env);
    {'ES:LAMBDA', Formals, Body} ->
      interpret_lambda(Formals, Body, Env);
    {'ES:LET', Bindings, Body} ->
      interpret_let(Bindings, Body, Env);
    {'ES:LETREC', Bindings, Body} ->
      interpret_letrec(Bindings, Body, Env);
    {'ES:LOCVAR', Var} ->
      interpret_locvar(Var, Env);
    {'ES:PRIMOP', PrimOp, Actuals} ->
      interpret_primop(PrimOp, Actuals, Env);
    {'ES:QUOTE', Value} ->
      interpret_quote(Value);
    {'ES:TRY', Expr, Var, Body, EVar, Handler, After} ->
      interpret_try(Expr, Var, Body, EVar, Handler, After, Env);
    {'ES:TUPLE', Exprs} ->
      interpret_tuple(Exprs, Env)
  end.

interpret_begin(First, Next, Env) ->
  interpret(First, Env),
  interpret(Next, Env).

interpret_case(Expr, Clauses, Env) ->
  {Body, NewEnv} = interpret_clauses(Clauses, interpret(Expr, Env), Env),
  interpret(Body, NewEnv).

interpret_cons(Hd, Tl, Env) ->
  [interpret(Hd, Env) | interpret(Tl, Env)].

interpret_define(Var, Expr, Env) ->
  %% This is restricted, by macro-expansion and parsing, to the top-level.
  es_gloenv:enter_var(Var, interpret(Expr, Env)).

interpret_glovar(Var) ->
  es_gloenv:get_var(Var).

interpret_if(Pred, Then, Else, Env) ->
  interpret(case interpret(Pred, Env) of false -> Else; _ -> Then end, Env).

interpret_lambda(Formals, Body, Env) ->
  make_function(
    Formals,
    fun (Actuals) ->
      interpret_lambda_body(Formals, Actuals, Body, Env)
    end).

interpret_lambda_body(Formals, Actuals, Body, Env) ->
  interpret(Body, bind_formals(Formals, Actuals, Env)).

interpret_let(Bindings, Body, Env) ->
  interpret(Body, es_env:overlay(Env, interpret_let_bindings(Bindings, Env))).

interpret_let_bindings(Bindings, Env) ->
  lists:foldl(fun ({Var, Expr}, NestedEnv) ->
                es_env:enter(NestedEnv, Var, interpret(Expr, Env))
              end,
              es_env:empty(), Bindings).

interpret_letrec(Bindings, Body, Env) ->
  interpret(Body, es_env:overlay(Env, interpret_letrec_bindings(Bindings, Env))).

interpret_letrec_bindings(Bindings, Env) ->
  RecEnv2 = lists:foldl(fun ({Var, Formals, Body}, RecEnv1) ->
                          es_env:enter(RecEnv1, Var, {Formals, Body, Env})
                        end,
                        es_env:empty(), Bindings),
  unfold_recenv(RecEnv2).

unfold_recenv(RecEnv) ->
  es_env:map(RecEnv,
             fun (_Var, {Formals, Body, Env}) ->
                 make_function(
                   Formals,
                   fun (Actuals) ->
                     %% The recursive unfolding is delayed until the function is
                     %% applied, making it finite.
                     RecEnv2 = unfold_recenv(RecEnv),
                     Env2 = es_env:overlay(Env, RecEnv2),
                     interpret_lambda_body(Formals, Actuals, Body, Env2)
                   end)
             end).

interpret_locvar(Var, Env) ->
  es_env:get(Env, Var).

interpret_primop(PrimOp, Args0, Env) ->
  Args = [interpret(Arg, Env) || Arg <- Args0],
  case {PrimOp, Args} of
    {'ES:APPLY', [F | Rest]} -> apply(F, Rest);
    {'ES:COLON', [M, F, A]} -> fun M:F/A;
    {'ES:LIST', _} -> Args;
    {'ES:RAISE', [Exn]} -> es_datum:raise(Exn)
  end.

interpret_quote(Value) ->
  Value.

interpret_try(Expr, Var, Body, EVar, Handler, _After = [], Env) ->
  interpret_try(Expr, Var, Body, EVar, Handler, Env);
interpret_try(Expr, Var, Body, EVar, Handler, After, Env) ->
  try interpret_try(Expr, Var, Body, EVar, Handler, Env)
  after interpret(After, Env)
  end.

interpret_try(Expr, Var, Body, EVar, Handler, Env) ->
  try
    interpret(Expr, Env) of
      Res ->
        interpret(Body, bind_formals([Var], [Res], Env))
  catch Class:Reason:Stack ->
    interpret(Handler, bind_formals([EVar], [{Class, Reason, Stack}], Env))
  end.

interpret_tuple(Exprs, Env) ->
  list_to_tuple(lists:map(fun (Expr) -> interpret(Expr, Env) end, Exprs)).

%% Pattern matching ------------------------------------------------------------
%%
%% Patterns follow the Erlang convention of classifying variables as bindings if
%% not already bound in the pattern or in the surrounding scope, and as equality
%% constraints otherwise. Plain variable patterns are represented as (= Var _).

-define(nomatch, nomatch).

interpret_clauses([], _Val, _Env) -> ?nomatch;
interpret_clauses([Clause | Clauses], Val, Env) ->
  case interpret_clause(Clause, Val, Env) of
    ?nomatch -> interpret_clauses(Clauses, Val, Env);
    {_Body, _NewEnv} = Match -> Match
  end.

interpret_clause({Pat, Guard, Body}, Val, Env) ->
  case match_pat(Pat, Val, Env) of
    ?nomatch -> ?nomatch;
    NewEnv ->
      case interpret_guard(Guard, NewEnv) of
        true -> {Body, NewEnv};
        false -> ?nomatch
      end
  end.

interpret_guard(Guard, Env) ->
  try interpret(Guard, Env) of
    true -> true;
    _ -> false
  catch _:_ -> false
  end.

match_pat(Pat, Val, Env) ->
  case Pat of
    {'ES:BIND', Var, Pat2} -> % Var is not bound
      match_pat(Pat2, Val, es_env:enter(Env, Var, Val));
    {'ES:CONS', Hd, Tl} ->
      case Val of
        [X | Y] ->
          case match_pat(Hd, X, Env) of
            ?nomatch -> ?nomatch;
            Env2 -> match_pat(Tl, Y, Env2)
          end;
        _ -> ?nomatch
      end;
    {'ES:EQUAL', Var, Pat2} -> % Var is bound, may be global
      case Val == get_pat_var(Var, Env) of
        true -> match_pat(Pat2, Val, Env);
        false -> ?nomatch
      end;
    {'ES:QUOTE', Val2} ->
      case Val == Val2 of
        true -> Env;
        false -> ?nomatch
      end;
    {'ES:TUPLE', Pats} ->
      case is_tuple(Val) of
        true ->
	  case tuple_size(Val) =:= length(Pats) of
            true -> match_tuple(Pats, 1, Val, Env);
            false -> ?nomatch
          end;
        false -> ?nomatch
      end;
    'ES:WILD' ->
      Env
  end.

get_pat_var(Var, Env) ->
  case es_env:lookup(Env, Var) of
    {value, Val} -> Val;
    none -> es_gloenv:get_var(Var)
  end.

match_tuple([], _I, _Tuple, Env) -> Env;
match_tuple([Pat | Pats], I, Tuple, Env) ->
  case match_pat(Pat, element(I, Tuple), Env) of
    ?nomatch -> ?nomatch;
    Env2 -> match_tuple(Pats, I + 1, Tuple, Env2)
  end.

%% Auxiliary helpers -----------------------------------------------------------

bind_formals([], [], Env) ->
  Env;
bind_formals([F|Fs], [A|As], Env) ->
  bind_formals(Fs, As, es_env:enter(Env, F, A)).

make_function(Formals, BodyFn) ->
  %% Synthesize a function of the correct arity.
  %% This is ugly, but erl_eval.erl does the same thing.
  case length(Formals) of
    0 ->
      fun () ->
        BodyFn([])
      end;
    1 ->
      fun (A1) ->
        BodyFn([A1])
      end;
    2 ->
      fun (A1, A2) ->
        BodyFn([A1, A2])
      end;
    3 ->
      fun (A1, A2, A3) ->
        BodyFn([A1, A2, A3])
      end;
    4 ->
      fun (A1, A2, A3, A4) ->
        BodyFn([A1, A2, A3, A4])
      end;
    5 ->
      fun (A1, A2, A3, A4, A5) ->
        BodyFn([A1, A2, A3, A4, A5])
      end;
    6 ->
      fun (A1, A2, A3, A4, A5, A6) ->
        BodyFn([A1, A2, A3, A4, A5, A6])
      end;
    7 ->
      fun (A1, A2, A3, A4, A5, A6, A7) ->
        BodyFn([A1, A2, A3, A4, A5, A6, A7])
      end;
    8 ->
      fun (A1, A2, A3, A4, A5, A6, A7, A8) ->
        BodyFn([A1, A2, A3, A4, A5, A6, A7, A8])
      end;
    9 ->
      fun (A1, A2, A3, A4, A5, A6, A7, A8, A9) ->
        BodyFn([A1, A2, A3, A4, A5, A6, A7, A8, A9])
      end;
    10 ->
      fun (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ->
        BodyFn([A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])
      end;
    Arity ->
      throw({argument_limit, Arity})
  end.
