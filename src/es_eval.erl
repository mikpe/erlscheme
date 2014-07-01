%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014 Mikael Pettersson
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

-module(es_eval).

-export([dynamic_eval/1, primitive_eval/1]).

%%

dynamic_eval(Sexpr) ->
  case es_gloenv:lookup('eval', 'var') of
    {value, Val} -> eval_call(Val, [Sexpr])
  end.

primitive_eval(Sexpr) ->
  eval(Sexpr, env_empty(), true).

%%

eval(Sexpr, Env) -> eval(Sexpr, Env, false).

eval(Sexpr, Env, IsToplevel) ->
  case Sexpr of
    [Hd | Tl] ->
      eval_form(Hd, Tl, Env, IsToplevel);
    Symbol when is_atom(Symbol) ->
      eval_atom(Symbol, Env);
    _ ->
      case is_self_evaluating(Sexpr) of
	true ->
	  Sexpr;
	false ->
	  erlang:throw({invalid_expression,Sexpr})
      end
  end.

eval_form(Hd, Tl, Env, IsTopLevel) ->
  case Hd of
    'and' ->
      eval_and(Tl, Env);
    'begin' ->
      eval_begin(Tl, Env);
    'define' ->
      eval_define(Tl, Env, IsTopLevel);
    'if' ->
      eval_if(Tl, Env);
    'lambda' ->
      eval_lambda(Tl, Env);
    'let' ->
      eval_let(Tl, Env);
    'letrec' ->
      eval_letrec(Tl, Env);
    'quote' ->
      eval_quote(Tl);
    _ ->
      eval_call(eval(Hd, Env),
		lists:map(fun (X) -> eval(X, Env) end, Tl))
  end.

eval_and(Tl, Env) ->
  case Tl of
    [Expr] ->
      eval(Expr, Env);
    [Expr | Rest] ->
      case eval(Expr, Env) of
	false -> false;
	_ -> eval_and(Rest, Env)
      end;
    [] ->
      true
  end.

eval_begin(Tl, Env) ->
  case Tl of
    [Expr] ->
      eval(Expr, Env);
    [Expr | Rest] ->
      eval(Expr, Env),
      eval_begin(Rest, Env);
    %% XXX: (begin) is valid at the top-level but not in expressions
    [] -> erlang:throw({bad_begin, Tl})
  end.

eval_let(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      eval(Body, env_overlay(Env, eval_let_bindings(Bindings, Env)));
    _ -> erlang:throw({bad_let, Tl})
  end.

eval_let_bindings(Bindings, Env) ->
  lists:foldl(fun (Binding, NestedEnv) ->
		  eval_let_binding(Binding, Env, NestedEnv)
	      end,
	      env_empty(), Bindings).

eval_let_binding([Var, Expr], Env, NestedEnv) ->
  env_enter(NestedEnv, Var, eval(Expr, Env)).

eval_letrec(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      eval(Body, env_overlay(Env, eval_letrec_bindings(Bindings, Env)));
    _ -> erlang:throw({bad_letrec, Tl})
  end.

eval_letrec_bindings(Bindings, Env) ->
  RecEnv2 = lists:foldl(fun (Binding, RecEnv1) ->
			    eval_letrec_binding(Binding, Env, RecEnv1)
			end,
			env_empty(), Bindings),
  unfold_recenv(RecEnv2).

unfold_recenv(RecEnv) ->
  env_map(RecEnv,
	  fun(_Var, {'ES:CLOSURE', F, B, E, _}) -> {'ES:CLOSURE', F, B, E, RecEnv} end).

eval_letrec_binding([Var, ['lambda' | Tl]], Env, RecEnv) when is_atom(Var) ->
  env_enter(RecEnv, Var, eval_lambda(Tl, Env)). % Var bound to closure with empty RecEnv

eval_lambda(Tl, Env) ->
  case Tl of
    [Formals, Body] -> {'ES:CLOSURE', Formals, Body, Env, env_empty()};
    _ -> erlang:throw({bad_lambda, Tl})
  end.

eval_define(Tl, Env, true) ->
  case Tl of
    [Var, Expr] ->
      es_gloenv:insert(Var, 'var', eval(Expr, Env))
  end.

eval_call(F, Args) ->
  case F of
    {'ES:ERLFUNC', Fun} ->
      Fun(Args);
    {'ES:CLOSURE', Formals, Body, Env, RecEnv} ->
      apply_closure(Formals, Body, Env, RecEnv, Args)
  end.

apply_closure(Formals, Body, Env, RecEnv, Args) ->
  RecEnv2 = unfold_recenv(RecEnv),
  Env2 = env_overlay(Env, RecEnv2),
  Env3 = bind_formals(Formals, Args, Env2),
  eval(Body, Env3).

bind_formals([], [], Env) ->
  Env;
bind_formals([F|Fs], [A|As], Env) when is_atom(F) ->
  bind_formals(Fs, As, env_enter(Env, F, A));
bind_formals(F, As, Env) when is_atom(F) ->
  env_enter(Env, F, As).

eval_if(Tl, Env) ->
  case Tl of
    [Pred, Then, Else] -> eval_if(Pred, Then, Else, Env);
    [Pred, Then] -> eval_if(Pred, Then, false, Env);
    _ -> erlang:throw({invalid_if, Tl})
    end.

eval_if(Pred, Then, Else, Env) ->
  eval(case eval(Pred, Env) of false -> Else; _ -> Then end, Env).

eval_quote(Tl) ->
  case Tl of
    [Sexpr] -> Sexpr;
    _ -> erlang:throw({invalid_quote, Tl})
  end.

eval_atom(Atom, Env) ->
  case Atom of
    true -> true;
    false -> false;
    _ ->
      case env_lookup(Env, Atom) of
	{value, Val} -> Val;
	none ->
	  case es_gloenv:lookup(Atom, 'var') of
	    {value, Val} -> Val;
	    none -> erlang:throw({unbound_variable, Atom})
	  end
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

%%

env_empty() -> gb_trees:empty().
env_lookup(Env, Var) -> gb_trees:lookup(Var, Env).
env_enter(Env, Var, Val) -> gb_trees:enter(Var, Val, Env).

env_map(Env, Fn) -> gb_trees:map(Fn, Env).

env_overlay(Env1, Env2) ->
  env_overlay_iter(Env1, gb_trees:iterator(Env2)).
env_overlay_iter(Env, Iter1) ->
  case gb_trees:next(Iter1) of
    none ->
      Env;
    {Var, Val, Iter2} ->
      env_overlay_iter(gb_trees:enter(Var, Val, Env), Iter2)
  end.
