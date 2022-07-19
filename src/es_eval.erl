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
%%%
%%% Extensions:
%%% - (: M F A) is equivalent to Erlang's fun M:F/A

-module(es_eval).

-export([dynamic_eval/1, primitive_eval/1, do_apply/2]).

%%

dynamic_eval(Sexpr) ->
  do_apply(es_gloenv:get_var('eval'), [Sexpr]).

primitive_eval(Sexpr) ->
  interpret(es_parse:toplevel(Sexpr), es_env:empty()).

%% AST interpreter

interpret(AST, Env) ->
  case AST of
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
    {'ES:SEQ', First, Next} ->
      interpret_seq(First, Next, Env);
    {'ES:QUOTE', Value} ->
      interpret_quote(Value)
  end.

do_apply(FVal, Actuals) ->
  es_apply:applyN(FVal, Actuals).

interpret_define(Var, Expr, Env) ->
  %% This is restricted, by macro-expansion and parsing, to the top-level.
  %% XXX: ensure we return a valid Scheme datum here
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

interpret_let(Bindings, Body, Env) ->
  interpret(Body, es_env:overlay(Env, interpret_let_bindings(Bindings, Env))).

interpret_let_bindings(Bindings, Env) ->
  lists:foldl(fun (Binding, NestedEnv) ->
		  interpret_let_binding(Binding, Env, NestedEnv)
	      end,
	      es_env:empty(), Bindings).

interpret_let_binding({Var, Expr}, Env, NestedEnv) ->
  es_env:enter(NestedEnv, Var, interpret(Expr, Env)).

interpret_letrec(Bindings, Body, Env) ->
  interpret(Body, es_env:overlay(Env, interpret_letrec_bindings(Bindings, Env))).

interpret_letrec_bindings(Bindings, Env) ->
  RecEnv2 = lists:foldl(fun (Binding, RecEnv1) ->
			    interpret_letrec_binding(Binding, Env, RecEnv1)
			end,
			es_env:empty(), Bindings),
  unfold_recenv(RecEnv2).

unfold_recenv(RecEnv) ->
  es_env:map(RecEnv,
	     fun (_Var, {Formals, Body, Env}) ->
		 make_function(
		   Formals,
		   fun (Actuals) ->
		     RecEnv2 = unfold_recenv(RecEnv),
		     Env2 = es_env:overlay(Env, RecEnv2),
		     interpret_lambda_body(Formals, Actuals, Body, Env2)
		   end)
	     end).

interpret_letrec_binding({Var, Formals, Body}, Env, RecEnv) ->
  es_env:enter(RecEnv, Var, {Formals, Body, Env}).

interpret_locvar(Var, Env) ->
  es_env:get(Env, Var).

interpret_primop(PrimOp, Args0, Env) ->
  Args = [interpret(Arg, Env) || Arg <- Args0],
  case {PrimOp, Args} of
    {'ES:APPLY', [F | Rest]} -> do_apply(F, Rest);
    {'ES:COLON', [M, F, A]} -> fun M:F/A;
    {'ES:LIST', _} -> Args
  end.

interpret_seq(First, Next, Env) ->
  interpret(First, Env),
  interpret(Next, Env).

interpret_quote(Value) ->
  Value.
