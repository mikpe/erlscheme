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
%%% es_parse.erl
%%%
%%% Parses a top-level S-expression and converts it to an abstract syntax
%%% tree (AST).
%%%
%%% Extensions:
%%% - (lambda M:F/A) evaluates to the function F of arity A exported from module M,
%%%   M, F, and A are all evaluated except that if M or F are unbound variables, they
%%%   are implicitly quoted to become literal symbols
%%% - (M:F A1 ... An) is equivalent to ((lambda M:F/n) A1 ... An)

-module(es_parse).

-export([toplevel/1]).

toplevel(Sexpr) ->
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
    _ when is_number(Sexpr) -> true; % includes characters
    true -> true;
    false -> true;
    {} -> true; % eof-object
    _ when is_binary(Sexpr) -> true; % strings
    %% bytevectors (since R6RS/R7RS) are also self-evaluating
    _ when element(1, Sexpr) =:= 'ES:VECTOR' -> true; % since R7RS
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

parse_call(Hd0, Tl, Env) ->
  Hd = parse(Hd0, Env),
  {Fun, Args1} =
    case Tl of
      [':', F0 | Args0] ->
        M = quote_if_glovar(Hd),
        F = quote_if_glovar(parse(F0, Env)),
        A = {'ES:QUOTE', length(Args0)},
        {{'ES:PRIMOP', 'ES:COLON', [M, F, A]}, Args0};
      _ ->
        {Hd, Tl}
    end,
  Args = [parse(Arg, Env) || Arg <- Args1],
  %% Recognize calls to special built-ins:
  %% - (list ...) becomes a built-in to avoid needing a variadic list/N function
  {PrimOp, PrimArgs} =
    case Fun of
      {'ES:GLOVAR', 'list'} -> {'ES:LIST', Args};
      _ -> {'ES:APPLY', [Fun | Args]}
    end,
  {'ES:PRIMOP', PrimOp, PrimArgs}.

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
    [M, ':', F, '/', A] ->
      {'ES:PRIMOP', 'ES:COLON', [quote_if_glovar(parse(M, Env)), quote_if_glovar(parse(F, Env)), parse(A, Env)]};
    [Formals, Body] ->
      ScopeEnv = parse_formals(Formals, es_env:empty()),
      {'ES:LAMBDA', Formals, parse(Body, es_env:overlay(Env, ScopeEnv))};
    _ ->
      erlang:throw({bad_lambda, Tl})
  end.

quote_if_glovar(AST) ->
  case AST of
    {'ES:GLOVAR', Atom} -> {'ES:QUOTE', Atom};
    _ -> AST
  end.

parse_formals(Formals, ScopeEnv) ->
  case Formals of
    [] ->
      ScopeEnv;
    [Formal | RestFormals] when is_atom(Formal) ->
      parse_formals(RestFormals, bind(Formal, ScopeEnv));
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
