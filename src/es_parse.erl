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
%%% - (set! ...) is restricted to assigning global variables in the REPL.
%%%
%%% Extensions:
%%% - (lambda M:F/A) evaluates to the function F of arity A exported from module M,
%%%   M, F, and A are all evaluated except that if M or F are unbound variables, they
%%%   are implicitly quoted to become literal symbols
%%% - (M:F A1 ... An) is equivalent to ((lambda M:F/n) A1 ... An)
%%% - (try ...) is the exception handling primitive, modelled after Erlang's try
%%% - (case ...) is the pattern-matching primitive, modelled after Erlang's case,
%%%   Scheme's (case ...) is not supported

-module(es_parse).

-export([ format_error/1
        , module/1
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
  parse(Sexpr, empty_repl_env(), true).

%% Internals: Modules ----------------------------------------------------------

parse_module_decl(Sexprs) ->
  case Sexprs of
    [['module', Name] | Rest] ->
      {Name, Rest};
    [X | _] ->
      parse_error({bad_module_decl, X});
    [] ->
      parse_error(bad_module_decl_missing)
  end.

parse_export_decl(Sexprs) ->
  case Sexprs of
    [['export' | Exports] | Rest] ->
      {parse_exports(Exports, []), Rest};
    _ ->
      parse_error(bad_export_decl_missing)
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
      parse_error({bad_export_decl, Exports})
  end.

check_exports(Exports, ModEnv) ->
  lists:foreach(fun (Export) -> check_export(Export, ModEnv) end, Exports).

check_export({F, A}, ModEnv) ->
  case es_env:lookup(ModEnv, F) of
    {value, A} -> ok;
    {value, B} -> parse_error({bad_export_arity, F, A, B});
    none -> parse_error({bad_export_undef, F, A})
  end.

parse_pre_defuns(Sexprs) ->
  lists:map(fun parse_pre_defun/1, Sexprs).

parse_pre_defun(Sexpr) ->
  case Sexpr of
    ['define', Name, ['lambda', Formals, Body]] when is_atom(Name) ->
      try length(Formals) of
        _Arity -> {Name, Formals, Body}
      catch error:badarg ->
        parse_error({bad_formals, Formals})
      end;
    _ ->
      parse_error({bad_defun, Sexpr})
  end.

build_modenv(PreDefuns) ->
  lists:foldl(fun build_modenv/2, empty_module_env(), PreDefuns).

build_modenv({Name, Formals, _Body}, Env) ->
  Arity = length(Formals),
  case es_env:is_bound(Env, Name) of
    true -> parse_error({bad_fun_binding, Name, Arity});
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
          parse_error({bad_expression, Sexpr})
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
    'case' ->
      parse_case(Tl, Env);
    'cond' ->
      parse_cond(Tl, Env);
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
    'set!' ->
      'parse_set!'(Tl, Env);
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
      parse_error({bad_and, Tl})
  end.

parse_atom(Atom, Env) ->
  parse_atom(Atom, Env, _QuoteIfGlovar = false).

parse_atom(Atom, Env, QuoteIfGlovar) ->
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
          if QuoteIfGlovar -> % this is the M or F in M:F
              {'ES:QUOTE', Atom};
             true ->
              {'ES:GLOVAR', Atom}
          end;
        none when QuoteIfGlovar -> % this is the M or F in M:F
          {'ES:QUOTE', Atom};
        none ->
          %% In modules replace global variables with the MFAs they denote.
          case is_module(Env) andalso es_env:lookup(es_lib_scheme_base:env(), Atom) of
            {value, Fun} when is_function(Fun) ->
              case erlang:fun_info(Fun, type) of
                {type, external} ->
                  {module, M} = erlang:fun_info(Fun, module),
                  {name, F} = erlang:fun_info(Fun, name),
                  {arity, A} = erlang:fun_info(Fun, arity),
                  {'ES:PRIMOP', 'ES:COLON', [{'ES:QUOTE', M}, {'ES:QUOTE', F}, {'ES:QUOTE', A}]};
                {type, local} ->
                  {'ES:GLOVAR', Atom}
              end;
            _ ->
              {'ES:GLOVAR', Atom}
          end
      end
  end.

parse_begin(Tl, Env, IsToplevel) ->
  case Tl of
    [Expr] ->
      parse(Expr, Env, IsToplevel);
    [Expr | Rest] ->
      {'ES:BEGIN', parse(Expr, Env, IsToplevel), parse_begin(Rest, Env, IsToplevel)};
    [] when IsToplevel ->       % (begin) is valid at the top-level
      {'ES:QUOTE', es_datum:unspecified()};
    _ ->
      parse_error({bad_begin, Tl})
  end.

parse_call(Hd, Tl, Env) ->
  {Fun, Args1} =
    case Tl of
      [':', F0 | Args0] ->
        M = quote_if_glovar(Hd, Env),
        F = quote_if_glovar(F0, Env),
        A = {'ES:QUOTE', length(Args0)},
        {{'ES:PRIMOP', 'ES:COLON', [M, F, A]}, Args0};
      _ ->
        {parse(Hd, Env), Tl}
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

parse_case(Tl, Env) ->
  case Tl of
    [Expr | Clauses] ->
      {'ES:CASE', parse(Expr, Env), parse_clauses(Clauses, default_case_clause(), Env)};
    _ ->
      parse_error({bad_case, Tl})
  end.

default_case_clause() ->
  %% Synthesize (_ (erlang:error 'case_clause))
  ErrorFun = {'ES:PRIMOP', 'ES:COLON', [{'ES:QUOTE', 'erlang'}, {'ES:QUOTE', 'error'}, {'ES:QUOTE', 1}]},
  ErrorExpr = {'ES:PRIMOP', 'ES:APPLY', [ErrorFun, {'ES:QUOTE', 'case_clause'}]},
  [{'ES:WILD', {'ES:QUOTE', 'true'}, ErrorExpr}].

parse_cond(Tl, Env) ->
  case Tl of
    [Clause | Clauses] ->
      parse_cond_clauses(Clause, Clauses, Env);
    _ ->
      parse_error({bad_cond, Tl})
  end.

parse_cond_clauses(['else' | Exprs], [], Env) ->
  %% (begin <Exprs>..)
  parse_begin(Exprs, Env, _IsToplevel = false);
parse_cond_clauses([Test], Rest, Env) ->
  %% (let ((<Var> <Test>)) (if <Var> <Var> (cond <Rest>..)))
  Var = newvar(),
  VarExp = {'ES:LOCVAR', Var},
  {'ES:LET', [{Var, parse(Test, Env)}],
   {'ES:IF', VarExp, VarExp, parse_cond_clauses(Rest, Env)}};
parse_cond_clauses([Test, '=>', Expr], Rest, Env) ->
  %% (let ((<Var> <Test>)) (if <Var> (<Expr> <Var>) (cond <Rest>..)))
  Var = newvar(),
  VarExp = {'ES:LOCVAR', Var},
  {'ES:LET', [{Var, parse(Test, Env)}],
   {'ES:IF', VarExp,
             {'ES:PRIMOP', 'ES:APPLY', [parse(Expr, Env), VarExp]},
             parse_cond_clauses(Rest, Env)}};
parse_cond_clauses([Test | Exprs], Rest, Env) ->
  %% (if <Test> (begin <Exprs>..) (cond <Rest>..))
  {'ES:IF', parse(Test, Env),
            parse_begin(Exprs, Env, _IsToplevel = false),
            parse_cond_clauses(Rest, Env)}.

parse_cond_clauses([Clause | Rest], Env) -> parse_cond_clauses(Clause, Rest, Env);
parse_cond_clauses([], _Env) -> {'ES:QUOTE', es_datum:unspecified()}.

parse_define(Tl, Env, IsToplevel) ->
  case {Tl, IsToplevel} of
    {[Var, Expr], true} when is_atom(Var) ->
      {'ES:DEFINE', Var, parse(Expr, Env)};
    _ ->
      parse_error({bad_define, Tl})
  end.

parse_if(Tl, Env) ->
  case Tl of
    [Pred, Then, Else] ->
      {'ES:IF', parse(Pred, Env), parse(Then, Env), parse(Else, Env)};
    [Pred, Then] ->
      {'ES:IF', parse(Pred, Env), parse(Then, Env), {'ES:QUOTE', es_datum:unspecified()}};
    _ ->
      parse_error({bad_if, Tl})
  end.

parse_lambda(Tl, Env) ->
  case Tl of
    [M, ':', F, '/', A] ->
      {'ES:PRIMOP', 'ES:COLON', [quote_if_glovar(M, Env), quote_if_glovar(F, Env), parse(A, Env)]};
    [Formals, Body] ->
      parse_plain_lambda(Formals, Body, Env);
    _ ->
      parse_error({bad_lambda, Tl})
  end.

parse_plain_lambda(Formals, Body, Env) ->
  ScopeEnv = parse_formals(Formals, es_env:empty()),
  {'ES:LAMBDA', Formals, parse(Body, es_env:overlay(Env, ScopeEnv))}.

quote_if_glovar(Sexpr, Env) ->
  if is_atom(Sexpr) ->
       parse_atom(Sexpr, Env, _QuoteIfGlovar = true);
     true ->
       parse(Sexpr, Env)
  end.

parse_formals(Formals, ScopeEnv) ->
  case Formals of
    [] ->
      ScopeEnv;
    [Formal | RestFormals] when is_atom(Formal) ->
      parse_formals(RestFormals, bind(Formal, ScopeEnv));
    _ ->
      parse_error({bad_formals, Formals})
  end.

bind(Var, ScopeEnv) when is_atom(Var) ->
  case es_env:is_bound(ScopeEnv, Var) of
    true -> parse_error({bad_var_binding, Var});
    false -> es_env:enter(ScopeEnv, Var, [])
  end.

parse_let(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      {NewBindings, ScopeEnv} = parse_let_bindings(Bindings, Env),
      {'ES:LET', NewBindings, parse(Body, es_env:overlay(Env, ScopeEnv))};
    _ ->
      parse_error({bad_let, tl})
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
      parse_error({bad_let_binding, Binding})
  end.

bind_let({Var, _Expr}, ScopeEnv) ->
  bind(Var, ScopeEnv).

parse_letrec(Tl, Env) ->
  case Tl of
    [Bindings, Body] ->
      {NewBindings, NewEnv} = parse_letrec_bindings(Bindings, Env),
      {'ES:LETREC', NewBindings, parse(Body, NewEnv)};
    _ ->
      parse_error({bad_letrec, Tl})
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
      parse_error({bad_letrec_binding, Binding})
  end.

bind_letrec(Binding, ScopeEnv) ->
  case Binding of
    [Var, _Lambda] when is_atom(Var) ->
      bind(Var, ScopeEnv);
    _ ->
      parse_error({bad_letrec_binding, Binding})
  end.

parse_quote(Tl) ->
  case Tl of
    [Value] ->
      {'ES:QUOTE', Value};
    _ ->
      parse_error({bad_quote, Tl})
  end.

'parse_set!'(Tl, Env) ->
  case Tl of
    [Var, Expr] when is_atom(Var) ->
      case is_global_in_repl(Env, Var) of
        true ->
          {'ES:SET!', Var, parse(Expr, Env)};
        false ->
          parse_error({'bad_set!_disallowed', Var})
      end;
    _ ->
      parse_error({'bad_set!', Tl})
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
          parse_error({bad_try, Tl})
      end;
    [] ->
      parse_error({bad_try, Tl})
  end.

parse_try_clause(Tag, [[Tag, Var, Expr] | Rest], Env) when is_atom(Var) ->
  {{Var, parse(Expr, es_env:enter(Env, Var, []))}, Rest};
parse_try_clause(Tag, [[Tag | _] = Clause | _Rest], _Env) ->
  parse_error({bad_try_clause, Clause});
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

%% Pattern matching ------------------------------------------------------------

parse_clauses([Clause | Clauses], Tail, Env) ->
  [parse_clause(Clause, Env) | parse_clauses(Clauses, Tail, Env)];
parse_clauses([], Tail, _Env) ->
  Tail.

parse_clause(Clause, Env) ->
  case Clause of
    [Pat, ['when', Guard], Body] ->
      parse_clause(Pat, {ok, Guard}, Body, Env);
    [Pat, Body] ->
      parse_clause(Pat, false, Body, Env);
    _ ->
      parse_error({bad_clause, Clause})
  end.

parse_clause(Pat, MaybeGuard, Body, Env) ->
  {ParsedPat, ClauseEnv} = parse_pat(Pat, Env),
  ParsedGuard =
    case MaybeGuard of
      {ok, Guard} -> parse_guard(Guard, ClauseEnv);
      false -> {'ES:QUOTE', 'true'}
    end,
  {ParsedPat, ParsedGuard, parse(Body, ClauseEnv)}.

parse_pat(Sexpr, Env) ->
  case Sexpr of
    '_' ->
      {'ES:WILD', Env};
    Var when is_atom(Var) -> % unquoted x means the variable x
      case is_bound_in_pat(Env, Var) of
        true ->
          {{'ES:EQUAL', Var, 'ES:WILD'}, Env};
        false ->
          {{'ES:BIND', Var, 'ES:WILD'}, bind(Var, Env)}
      end;
    ['quote', Atom] when is_atom(Atom) -> % 'x means the symbol x
      {{'ES:QUOTE', Atom}, Env};
    ['=', Var, Pat2] when is_atom(Var), Var =/= '_' ->
      case is_bound_in_pat(Env, Var) of
        true ->
          {ParsedPat2, Env2} = parse_pat(Pat2, Env),
          {{'ES:EQUAL', Var, ParsedPat2}, Env2};
        false ->
          {ParsedPat2, Env2} = parse_pat(Pat2, bind(Var, Env)),
          {{'ES:BIND', Var, ParsedPat2}, Env2}
      end;
    [Pat1 | Pat2] ->
      {ParsedPat1, Env1} = parse_pat(Pat1, Env),
      {ParsedPat2, Env2} = parse_pat(Pat2, Env1),
      {{'ES:CONS', ParsedPat1, ParsedPat2}, Env2};
    Tuple when is_tuple(Tuple) ->
      parse_tuple_pat(tuple_to_list(Tuple), [], Env);
    [] ->
      {{'ES:QUOTE', []}, Env};
    _ ->
      case is_self_evaluating(Sexpr) of
        true ->
          {{'ES:QUOTE', Sexpr}, Env};
        false ->
          parse_error({bad_pattern, Sexpr})
      end
  end.

parse_tuple_pat([], ParsedPats, Env) ->
  {{'ES:TUPLE', lists:reverse(ParsedPats)}, Env};
parse_tuple_pat([Pat | Pats], ParsedPats, Env) ->
  {ParsedPat, NewEnv} = parse_pat(Pat, Env),
  parse_tuple_pat(Pats, [ParsedPat | ParsedPats], NewEnv).

%% Guards ----------------------------------------------------------------------

parse_guard(Sexpr, Env) ->
  Guard = parse(Sexpr, Env),
  check_guard(Guard),
  Guard.

check_guard(Guard) ->
  case Guard of
    {'ES:BEGIN', _, _} -> % only useful for side-effects
      invalid_guard(Guard);
    {'ES:CASE', _, _} -> % can fail
      invalid_guard(Guard);
    {'ES:CONS', Hd, Tl} ->
      check_guard(Hd),
      check_guard(Tl);
    {'ES:DEFINE', _, _} ->
      invalid_guard(Guard);
    {'ES:GLOVAR', _} ->
      ok;
    {'ES:IF', Pred, Then, Else} ->
      check_guard(Pred),
      check_guard(Then),
      check_guard(Else);
    {'ES:LAMBDA', _, _} -> % safe, but pointless since it can't be applied
      ok;
    {'ES:LET', Bindings, Body} ->
      lists:foreach(fun({_Lhs, Rhs}) -> check_guard(Rhs) end, Bindings),
      check_guard(Body);
    {'ES:LETREC', _, _} -> % can loop/recurse
      invalid_guard(Guard);
    {'ES:LOCVAR', _} ->
      ok;
    {'ES:PRIMOP', PrimOp, Args} ->
      case {PrimOp, Args} of
        {'ES:APPLY', [ {'ES:PRIMOP', 'ES:COLON', [{'ES:QUOTE', 'erlang'}, {'ES:QUOTE', F}, {'ES:QUOTE', A}]}
                     | Rest]} ->
          case A =:= length(Rest) andalso erl_internal:guard_bif(F, A) of
            true -> lists:foreach(fun check_guard/1, Rest);
            false -> invalid_guard(Guard)
          end;
        {'ES:COLON', _} -> % safe but pointless in isolation
          invalid_guard(Guard);
        {'ES:LIST', _} ->
          lists:foreach(fun check_guard/1, Args);
        {'ES:RAISE', _} ->
          invalid_guard(Guard)
      end;
    {'ES:QUOTE', _} ->
      ok;
    {'ES:SET!', _, _} ->
      invalid_guard(Guard);
    {'ES:TRY', _, _, _, _, _, _} ->
      invalid_guard(Guard);
    {'ES:TUPLE', Exprs} ->
      lists:foreach(fun check_guard/1, Exprs)
  end.

-dialyzer({no_return, invalid_guard/1}).
invalid_guard(Expr) ->
  parse_error({bad_guard, Expr}).

%% Auxiliary helpers -----------------------------------------------------------

%% The parse-time environment records lexically bound variables, and in modules
%% also the toplevel defuns. Global variables need to be considered bound in
%% in some contexts in the REPL, so the environment has a marker to distinguish
%% modules from the REPL.

-define(MARKER_KEY, []). % no ErlScheme variable has name []

empty_module_env() ->
  es_env:enter(es_env:empty(), ?MARKER_KEY, 'module').

empty_repl_env() ->
  es_env:enter(es_env:empty(), ?MARKER_KEY, 'repl').

%% For patterns in the REPL we need to classify global variables as bound.
is_bound_in_pat(Env, Var) ->
  es_env:is_bound(Env, Var) orelse
  (es_env:get(Env, ?MARKER_KEY) =:= 'repl' andalso
   es_gloenv:is_bound_var(Var)).

is_global_in_repl(Env, Var) ->
  (not es_env:is_bound(Env, Var)) andalso (es_env:get(Env, ?MARKER_KEY) =:= 'repl').

is_module(Env) ->
  es_env:get(Env, ?MARKER_KEY) =:= 'module'.

newvar() ->
  erlang:unique_integer([positive]).

%% Error Formatting ------------------------------------------------------------

parse_error(Reason) ->
  error({?MODULE, Reason}).

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {bad_module_decl, X} ->
      io_lib:format("invalid (module ..) declaration: ~p", [X]);
    bad_module_decl_missing ->
      "missing (module ..) declaration";
    bad_export_decl_missing ->
      "missing (export ..) declaration";
    {bad_export_decl, X} ->
      io_lib:format("invalid (export ..) declaration: ~p", [X]);
    {bad_export_arity, F, A, B} ->
      io_lib:format("bad export ~p: expected arity ~p, actual arity ~p", [F, A, B]);
    {bad_export_undef, F, A} ->
      io_lib:format("bad export of undefined ~p/~p", [F, A]);
    {bad_formals, X} ->
      io_lib:format("formal parameters is not a proper list: ~p", [X]);
    {bad_defun, X} ->
      io_lib:format("invalid toplevel function definition: ~p", [X]);
    {bad_fun_binding, F, A} ->
      io_lib:format("toplevel function ~p/~p: already defined", [F, A]);
    {bad_var_binding, V} ->
      io_lib:format("variable ~p already bound in local scope", [V]);
    {bad_expression, X} ->
      io_lib:format("invalid expression: ~p", [X]);
    {bad_and, X} ->
      io_lib:format("invalid (and ..) expression: ~p", [X]);
    {bad_begin, X} ->
      io_lib:format("invalid (begin ..) expression: ~p", [X]);
    {bad_case, X} ->
      io_lib:format("invalid (case ..) expression: ~p", [X]);
    {bad_cond, X} ->
      io_lib:format("invalid (cond ..) expression: ~p", [X]);
    {bad_define, X} ->
      io_lib:format("malformed or malplaced (define ..) expression: ~p", [X]);
    {bad_if, X} ->
      io_lib:format("invalid (if ..) expression: ~p", [X]);
    {bad_lambda, X} ->
      io_lib:format("invalid (lambda ..) expression: ~p", [X]);
    {bad_let, X} ->
      io_lib:format("invalid (let ..) expression: ~p", [X]);
    {bad_let_binding, X} ->
      io_lib:format("invalid binding in let: ~p", [X]);
    {bad_letrec, X} ->
      io_lib:format("invalid (letrec ..) expression: ~p", [X]);
    {bad_letrec_binding, X} ->
      io_lib:format("invalid binding in letrec: ~p", [X]);
    {bad_quote, X} ->
      io_lib:format("invalid (quote ..) expression: ~p", [X]);
    {'bad_set!', X} ->
      io_lib:format("invalid (set! ..) expression: ~p", [X]);
    {'bad_set!_disallowed', V} ->
      io_lib:format("not allowed: (set! ~p ..)", [V]);
    {bad_try, X} ->
      io_lib:format("invalid (try ..) expression: ~p", [X]);
    {bad_try_clause, X} ->
      io_lib:format("invalid (try ..) clause: ~p", [X]);
    {bad_clause, X} ->
      io_lib:format("invalid pattern-matching clause: ~p", [X]);
    {bad_pattern, X} ->
      io_lib:format("invalid pattern: ~p", [X]);
    {bad_guard, X} ->
      io_lib:format("invalid guard: ~p", [X]);
    _ ->
      io_lib:format("~p", [Reason])
  end.
