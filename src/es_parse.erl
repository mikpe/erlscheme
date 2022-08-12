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
%%% Checks bindings, variable references, and export declarations. (This is not
%%% postponed to a "lint" module since this processing is needed anyway in order
%%% to generate correct AST for variable references and ":" operands.)
%%%
%%% Notes:
%%% - Variadic functions (with "rest" parameters) are not supported, since
%%%   they mess up calling conventions and interoperability with Erlang.
%%%
%%% Extensions:
%%% - (lambda M:F/A) evaluates to the function F of arity A exported from module M,
%%%   M, F, and A are all evaluated except that if M or F are unbound variables, they
%%%   are implicitly quoted to become literal symbols
%%% - (M:F A1 ... An) is equivalent to ((lambda M:F/n) A1 ... An)
%%% - (try ...) is the exception handling primitive, modelled after Erlang's try

-module(es_parse).

-export([ module/1
        , toplevel/1
        ]).

-type sexpr() :: term().
-type ast() :: term().

%% API -------------------------------------------------------------------------

-spec module([sexpr()]) -> ast().
module(Sexprs) ->
  {Name, Sexprs1} = parse_module_decl(Sexprs),
  {Exports, Sexprs2} = parse_export_decl(Sexprs1),
  PreDefuns = parse_pre_defuns(Sexprs2),
  ModEnv = build_modenv(PreDefuns),
  Defuns = parse_defuns(PreDefuns, ModEnv),
  check_exports(Exports, ModEnv),
  {'ES:MODULE', Name, Exports, Defuns}.

-spec toplevel(sexpr()) -> ast().
toplevel(Sexpr) ->
  parse(Sexpr, es_env:empty(), true).

%% Internals: Modules ----------------------------------------------------------

parse_module_decl(Sexprs) ->
  case Sexprs of
    [['module', Name] | Rest] ->
      {Name, Rest};
    [X | _] ->
      erlang:throw({invalid_module_decl, X});
    [] ->
      erlang:throw(missing_module_decl)
  end.

parse_export_decl(Sexprs) ->
  case Sexprs of
    [['export' | Exports] | Rest] ->
      {parse_exports(Exports, []), Rest};
    _ ->
      erlang:throw(missing_export_decl)
  end.

parse_exports(Exports, Acc) ->
  case Exports of
    [] ->
      lists:reverse(Acc);
    [F, '/', A | Rest] when is_atom(F), is_integer(A), A >= 0 ->
      parse_exports(Rest, [{F, A} | Acc]);
    [['/', F, A] | Rest] when is_atom(F), is_integer(A), A >= 0 ->
      parse_exports(Rest, [{F, A} | Acc]);
    _ ->
      erlang:throw(invalid_export_decl)
  end.

check_exports(Exports, ModEnv) ->
  lists:foreach(fun (Export) -> check_export(Export, ModEnv) end, Exports).

check_export({F, A}, ModEnv) ->
  case es_env:lookup(ModEnv, F) of
    {value, A} -> ok;
    {value, B} -> erlang:throw({export_wrong_arity, F, A, B});
    none -> erlang:throw({export_undef, F, A})
  end.

parse_pre_defuns(Sexprs) ->
  lists:map(fun parse_pre_defun/1, Sexprs).

parse_pre_defun(Sexpr) ->
  case Sexpr of
    ['define', Name, ['lambda', Formals, Body]] when is_atom(Name) ->
      try length(Formals) of
        _Arity -> {Name, Formals, Body}
      catch error:badarg ->
        erlang:throw({bad_formals, Formals})
      end;
    _ ->
      erlang:throw({invalid_defun, Sexpr})
  end.

build_modenv(PreDefuns) ->
  lists:foldl(fun build_modenv/2, es_env:empty(), PreDefuns).

build_modenv({Name, Formals, _Body}, Env) ->
  Arity = length(Formals),
  case es_env:is_bound(Env, Name) of
    true -> erlang:throw({already_bound, Name, Arity});
    false -> es_env:enter(Env, Name, Arity)
  end.

parse_defuns(PreDefuns, ModEnv) ->
  lists:map(fun (PreDefun) -> parse_defun(PreDefun, ModEnv) end, PreDefuns).

parse_defun({Name, Formals, Body}, ModEnv) ->
  {'ES:DEFINE', Name, parse_plain_lambda(Formals, Body, ModEnv)}.

%% Internals: Expressions ------------------------------------------------------

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
    _ when is_binary(Sexpr) -> true; % strings
    %% bytevectors (since R6RS/R7RS) are also self-evaluating
    _ when is_tuple(Sexpr) -> true; % vectors are self-evaluating since R7RS
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
    'try' ->
      parse_try(Tl, Env);
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
        {value, []} ->
          {'ES:LOCVAR', Atom};
        {value, A} when is_integer(A) ->
          {'ES:GLOVAR', Atom};
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
    [] when IsToplevel ->       % (begin) is valid at the top-level
      {'ES:QUOTE', es_datum:unspecified()};
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
      {'ES:IF', parse(Pred, Env), parse(Then, Env), {'ES:QUOTE', es_datum:unspecified()}};
    _ ->
      erlang:throw({bad_if, Tl})
  end.

parse_lambda(Tl, Env) ->
  case Tl of
    [M, ':', F, '/', A] ->
      {'ES:PRIMOP', 'ES:COLON', [quote_if_glovar(parse(M, Env)), quote_if_glovar(parse(F, Env)), parse(A, Env)]};
    [Formals, Body] ->
      parse_plain_lambda(Formals, Body, Env);
    _ ->
      erlang:throw({bad_lambda, Tl})
  end.

parse_plain_lambda(Formals, Body, Env) ->
  ScopeEnv = parse_formals(Formals, es_env:empty()),
  {'ES:LAMBDA', Formals, parse(Body, es_env:overlay(Env, ScopeEnv))}.

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
  case es_env:is_bound(ScopeEnv, Var) of
    true -> erlang:throw({already_bound, Var});
    false -> es_env:enter(ScopeEnv, Var, [])
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

%% Erlang-like try construct:
%%
%% (try Expr
%%   (of Var Body)
%%   (catch EVar Handler)
%%   (after After))
%%
%% Expr is evaluated in a dynamic context with a new current exception handler.
%%
%% If Expr evaluates to Val without raising an exception, Body is evaluated with
%% Var bound to Val in the original context of the try, i.e. without the new
%% exception handler present, and the value of Body becomes the value of the try.
%% If (of Var Body) is absent it is treated as (of x x) for some fresh variable x.
%%
%% If the evaluation of Expr raises an exception E, Handler is evaluated with EVar
%% bound to E in the original context of the try, i.e. without the new exception
%% handler present, and the value of Handler becomes the value of the try.
%% If (catch Var Handler) is absent it is treated as (catch x (raise x)) for some
%% fresh variable x.
%%
%% If (after After) is present, After is evaluated and its value ignored in the
%% original context of the try, i.e. without the new handler present, immediately
%% before the try returns or raises to the original context of the try.
%%
%% The of, catch, and after clauses are all optional, but at least one of catch
%% or after must be present.
parse_try(Tl, Env) ->
  case Tl of
    [Expr0 | RestExpr] ->
      Expr = parse(Expr0, Env),
      {MaybeOf, RestOf} = parse_try_clause('of', RestExpr, Env),
      {MaybeCatch, RestHandle} = parse_try_clause('catch', RestOf, Env),
      {After, RestAfter} = parse_try_after(RestHandle, Env),
      case (RestAfter =:= []) andalso (MaybeCatch =/= [] orelse After =/= []) of
        true ->
          {Var, Body} = fixup_try_of(MaybeOf),
          {EVar, Handler} = fixup_try_catch(MaybeCatch),
          {'ES:TRY', Expr, Var, Body, EVar, Handler, After};
        false ->
          erlang:throw({bad_try, Tl})
      end;
    [] ->
      erlang:throw({bad_try, Tl})
  end.

parse_try_clause(Tag, [[Tag, Var, Expr] | Rest], Env) when is_atom(Var) ->
  {{Var, parse(Expr, es_env:enter(Env, Var, []))}, Rest};
parse_try_clause(Tag, [[Tag | _] = Clause | _Rest], _Env) ->
  erlang:throw({bad_try_clause, Clause});
parse_try_clause(_Tag, Rest, _Env) ->
  {[], Rest}.

parse_try_after([['after', Expr] | Rest], Env) ->
  {parse(Expr, Env), Rest};
parse_try_after(Rest, _Env) ->
  {[], Rest}.

fixup_try_of(MaybeOf) ->
  case MaybeOf of
    {_Var, _Body} -> MaybeOf;
    [] ->
      Var = newvar(),
      {Var, {'ES:LOCVAR', Var}}
  end.

fixup_try_catch(MaybeCatch) ->
  case MaybeCatch of
    {_EVar, _Handler} -> MaybeCatch;
    [] ->
      EVar = newvar(),
      Handler = {'ES:PRIMOP', 'ES:RAISE', [{'ES:LOCVAR', EVar}]},
      {EVar, Handler}
  end.

newvar() ->
  erlang:unique_integer([positive]).
