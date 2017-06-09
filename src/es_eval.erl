%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014-2017 Mikael Pettersson
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

-module(es_eval).

-export([dynamic_eval/1, primitive_eval/1]).

%%

dynamic_eval(Sexpr) ->
  case es_gloenv:lookup('eval', 'var') of
    {value, Val} -> do_apply(Val, [Sexpr])
  end.

primitive_eval(Sexpr) ->
  interpret(parse_toplevel(Sexpr), es_env:empty()).

%% Parser from S-expressions to ASTs

parse_toplevel(Sexpr) ->
  parse(Sexpr, es_env:empty(), true).

parse(Sexpr, Env) ->
  parse(Sexpr, Env, false).

parse(Sexpr, Env, IsToplevel) ->
  case Sexpr of
    [Hd | Tl] ->
      parse_form(Hd, Tl, Env, IsToplevel);
    Symbol when is_atom(Symbol) ->
      parse_atom(Symbol, Env);
    _ ->
      case is_self_evaluating(Sexpr) of
	true ->
	  {'ES:QUOTE', Sexpr};
	false ->
	  erlang:throw({invalid_expression, Sexpr})
      end
  end.

is_self_evaluating(Sexpr) ->
  case Sexpr of
    _ when is_number(Sexpr) -> true;
    true -> true;
    false -> true;
    {'ES:CHAR', _Ch} -> true;
    {} -> true; % eof-object
    {'ES:STRING', _Bin} -> true;
    {'ES:BYTEVECTOR', _Bin} -> true;
    _ -> false
  end.

parse_form(Hd, Tl, Env, IsToplevel) ->
  case Hd of
    'and' ->
      parse_and(Tl, Env);
    'begin' ->
      parse_begin(Tl, Env, IsToplevel);
    'define' ->
      parse_define(Tl, Env, IsToplevel);
    'if' ->
      parse_if(Tl, Env);
    'lambda' ->
      parse_lambda(Tl, Env);
    'let' ->
      parse_let(Tl, Env);
    'letrec' ->
      parse_letrec(Tl, Env);
    'quote' ->
      parse_quote(Tl);
    _ ->
      parse_call(Hd, Tl, Env)
  end.

parse_and(Tl, Env) ->
  case Tl of
    [Expr] ->
      parse(Expr, Env);
    [Expr | Rest] ->
      {'ES:IF', parse(Expr, Env), parse_and(Rest, Env), {'ES:QUOTE', false}};
    [] ->
      {'ES:QUOTE', true};
    _ ->
      erlang:throw({bad_and, Tl})
  end.

parse_atom(Atom, Env) ->
  case Atom of
    true ->
      {'ES:QUOTE', Atom};
    false ->
      {'ES:QUOTE', Atom};
    _ ->
      case es_env:lookup(Env, Atom) of
	{value, _} ->
	  {'ES:LOCVAR', Atom};
	none ->
	  {'ES:GLOVAR', Atom}
      end
  end.

parse_begin(Tl, Env, IsToplevel) ->
  case Tl of
    [Expr] ->
      parse(Expr, Env, IsToplevel);
    [Expr | Rest] ->
      {'ES:SEQ', parse(Expr, Env, IsToplevel), parse_begin(Rest, Env, IsToplevel)};
    [] when IsToplevel ->	% (begin) is valid at the top-level
      {'EQ:QUOTE', []};		% XXX: #!undefined
    _ ->
      erlang:throw({bad_begin, Tl})
  end.

parse_call(Fun, Args, Env) ->
  {'ES:CALL', parse(Fun, Env), [parse(Arg, Env) || Arg <- Args]}.

parse_define(Tl, Env, IsToplevel) ->
  case {Tl, IsToplevel} of
    {[Var, Expr], true} when is_atom(Var) ->
      {'ES:DEFINE', Var, parse(Expr, Env)};
    _ ->
      erlang:throw({bad_define, Tl})
  end.

parse_if(Tl, Env) ->
  case Tl of
    [Pred, Then, Else] ->
      {'ES:IF', parse(Pred, Env), parse(Then, Env), parse(Else, Env)};
    [Pred, Then] ->
      {'ES:IF', parse(Pred, Env), parse(Then, Env), {'ES:QUOTE', []}}; % XXX: #!undefined
    _ ->
      erlang:throw({bad_if, Tl})
  end.

parse_lambda(Tl, Env) ->
  case Tl of
    [Formals, Body] ->
      ScopeEnv = parse_formals(Formals, es_env:empty()),
      {'ES:LAMBDA', Formals, parse(Body, es_env:overlay(Env, ScopeEnv))};
    _ ->
      erlang:throw({bad_lambda, Tl})
  end.

parse_formals(Formals, ScopeEnv) ->
  case Formals of
    [] ->
      ScopeEnv;
    [Formal | RestFormals] when is_atom(Formal) ->
      parse_formals(RestFormals, bind(Formal, ScopeEnv));
    RestFormal when is_atom(RestFormal) ->
      bind(RestFormal, ScopeEnv);
    _ ->
      erlang:throw({bad_formals, Formals})
  end.

bind(Var, ScopeEnv) when is_atom(Var) ->
  case es_env:lookup(ScopeEnv, Var) of
    none ->
      es_env:enter(ScopeEnv, Var, []);
    _ ->
      erlang:throw({bad_var, Var})
  end.

parse_let(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      {NewBindings, ScopeEnv} = parse_let_bindings(Bindings, Env),
      {'ES:LET', NewBindings, parse(Body, es_env:overlay(Env, ScopeEnv))};
    _ ->
      erlang:throw({bad_let, tl})
  end.

parse_let_bindings(Bindings, Env) ->
  NewBindings = [parse_let_binding(Binding, Env) || Binding <- Bindings],
  ScopeEnv = lists:foldl(fun bind_let/2, es_env:empty(), NewBindings),
  {NewBindings, ScopeEnv}.

parse_let_binding(Binding, Env) ->
  case Binding of
    [Var, Expr] when is_atom(Var) ->
      {Var, parse(Expr, Env)};
    _ ->
      erlang:throw({bad_let_binding, Binding})
  end.

bind_let({Var, _Expr}, ScopeEnv) ->
  bind(Var, ScopeEnv).

parse_letrec(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      {NewBindings, NewEnv} = parse_letrec_bindings(Bindings, Env),
      {'ES:LETREC', NewBindings, parse(Body, NewEnv)};
    _ ->
      erlang:throw({bad_letrec, Tl})
  end.

parse_letrec_bindings(Bindings, Env) ->
  ScopeEnv = lists:foldl(fun bind_letrec/2, es_env:empty(), Bindings),
  NewEnv = es_env:overlay(Env, ScopeEnv),
  NewBindings = [parse_letrec_binding(Binding, NewEnv) || Binding <- Bindings],
  {NewBindings, NewEnv}.

parse_letrec_binding(Binding, NewEnv) ->
  case Binding of
    [Var, ['lambda', Formals, Body]] when is_atom(Var) ->
      ScopeEnv = parse_formals(Formals, es_env:empty()),
      {Var, Formals, parse(Body, es_env:overlay(NewEnv, ScopeEnv))};
    _ ->
      erlang:throw({bad_letrec_binding, Binding})
  end.

bind_letrec(Binding, ScopeEnv) ->
  case Binding of
    [Var, _Lambda] when is_atom(Var) ->
      bind(Var, ScopeEnv);
    _ ->
      erlang:throw({bad_letrec_binding, Binding})
  end.

parse_quote(Tl) ->
  case Tl of
    [Value] ->
      {'ES:QUOTE', Value};
    _ ->
      erlang:throw({bad_quote, Tl})
  end.

%% AST interpreter

interpret(AST, Env) ->
  case AST of
    {'ES:CALL', Fun, Actuals} ->
      interpret_call(Fun, Actuals, Env);
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
    {'ES:SEQ', First, Next} ->
      interpret_seq(First, Next, Env);
    {'ES:QUOTE', Value} ->
      interpret_quote(Value)
  end.

interpret_call(Fun, Args, Env) ->
  do_apply(interpret(Fun, Env), [interpret(Arg, Env) || Arg <- Args]).

do_apply(FVal, Actuals) ->
  case FVal of
    {'ES:ERLFUNC', Fun} ->
      Fun(Actuals);
    {'ES:CLOSURE', Formals, Body, Env, RecEnv} ->
      RecEnv2 = unfold_recenv(RecEnv),
      Env2 = es_env:overlay(Env, RecEnv2),
      Env3 = bind_formals(Formals, Actuals, Env2),
      interpret(Body, Env3)
  end.

bind_formals([], [], Env) ->
  Env;
bind_formals([F|Fs], [A|As], Env) ->
  bind_formals(Fs, As, es_env:enter(Env, F, A));
bind_formals(F, As, Env) when is_atom(F) -> % rest parameter
  es_env:enter(Env, F, As).

interpret_define(Var, Expr, Env) ->
  %% This is restricted, by macro-expansion and parsing, to the top-level.
  %% XXX: ensure we return a valid Scheme datum here
  es_gloenv:insert(Var, 'var', interpret(Expr, Env)).

interpret_glovar(Var) ->
  case es_gloenv:lookup(Var, 'var') of
    {value, Value} ->
      Value;
    none ->
      erlang:throw({unbound_variable, Var})
  end.

interpret_if(Pred, Then, Else, Env) ->
  interpret(case interpret(Pred, Env) of false -> Else; _ -> Then end, Env).

interpret_lambda(Formals, Body, Env) ->
  {'ES:CLOSURE', Formals, Body, Env, es_env:empty()}.

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
	     fun(_Var, {'ES:CLOSURE', F, B, E, _}) ->
		 {'ES:CLOSURE', F, B, E, RecEnv}
	     end).

interpret_letrec_binding({Var, Formals, Body}, Env, RecEnv) ->
  %% initially Var gets bound to a closure with an empty RecEnv
  es_env:enter(RecEnv, Var, interpret_lambda(Formals, Body, Env)).

interpret_locvar(Var, Env) ->
  es_env:get(Env, Var).

interpret_seq(First, Next, Env) ->
  interpret(First, Env),
  interpret(Next, Env).

interpret_quote(Value) ->
  Value.
