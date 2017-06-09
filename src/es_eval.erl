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
  interpret(es_parse:toplevel(Sexpr), es_env:empty()).

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
