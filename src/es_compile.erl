%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2017-2022 Mikael Pettersson
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
%%% es_compile.erl
%%%
%%% Compiles ErlScheme modules to Core Erlang.

-module(es_compile).

-export([ file/1
        ]).

-type datum() :: term().

%% API -------------------------------------------------------------------------

-spec file(datum()) -> ok.
file(Arg) ->
  file(Arg, _Opts = []).

-spec file(datum(), proplists:proplist()) -> ok.
file(Arg, Opts) ->
  FileName = binary_to_list(es_datum:string_to_binary(Arg)),
  AST = es_load:module(FileName),
  BaseName = filename:basename(FileName, ".scm"),
  case proplists:get_bool(save_ast, Opts) of
    true ->
      ok = file:write_file(BaseName ++ ".ast", io_lib:format("~p\n", [AST]));
    false ->
      ok
  end,
  CerlModule = translate_module(AST),
  {ok, _} = core_lint:module(CerlModule),
  CoreFile = BaseName ++ ".core",
  ok = file:write_file(CoreFile, io_lib:format("~s\n", [core_pp:format(CerlModule)])),
  {ok, _} = compile:file(CoreFile, [from_core, verbose, report_errors, report_warnings]),
  ok.

%% Internals -------------------------------------------------------------------

translate_module({'ES:MODULE', ModuleName, Exports, Defuns}) ->
  CerlModuleName = cerl:c_atom(ModuleName),
  %% Exports: add module_info/0 and module_info/1
  CerlExports = [modinfo0_fname(), modinfo1_fname() | lists:map(fun translate_export/1, Exports)],
  %% Compute a mapping FName -> Arity from the top-level defuns
  FEnv = build_fenv(Defuns),
  %% Translate each top-level (define (f ...) ...)
  %% Also define module_info/0 and module_info/1.
  CerlDefuns = [modinfo0_def(CerlModuleName), modinfo1_def(CerlModuleName) |
                lists:map(fun(Defun) -> translate_defun(Defun, FEnv) end, Defuns)],
  %% Assemble the module.
  cerl:c_module(CerlModuleName, CerlExports, CerlDefuns).

translate_export({F, A}) ->
  cerl:c_fname(F, A).

build_fenv(Defuns) ->
  lists:foldl(fun build_fenv/2, es_env:empty(), Defuns).

build_fenv({'ES:DEFINE', Var, {'ES:LAMBDA', Formals, _Body}}, FEnv) ->
  none = es_env:lookup(FEnv, Var), % assert
  es_env:enter(FEnv, Var, length(Formals)).

translate_defun({'ES:DEFINE', Var, {'ES:LAMBDA', Formals, Body}}, FEnv) ->
  CerlFVar = cerl:c_fname(Var, length(Formals)),
  CerlFun = translate_lambda(Formals, Body, FEnv),
  {CerlFVar, CerlFun}.

translate_expr(AST, FEnv) ->
  case AST of
    {'ES:GLOVAR', Var} ->
      translate_glovar(Var, FEnv);
    {'ES:IF', Pred, Then, Else} ->
      translate_if(Pred, Then, Else, FEnv);
    {'ES:LAMBDA', Formals, Body} ->
      translate_lambda(Formals, Body, FEnv);
    {'ES:LET', Bindings, Body} ->
      translate_let(Bindings, Body, FEnv);
    {'ES:LETREC', Bindings, Body} ->
      translate_letrec(Bindings, Body, FEnv);
    {'ES:LOCVAR', Var} ->
      translate_locvar(Var);
    {'ES:PRIMOP', PrimOp, Actuals} ->
      translate_primop(PrimOp, Actuals, FEnv);
    {'ES:QUOTE', Value} ->
      translate_quote(Value);
    {'ES:SEQ', First, Next} ->
      translate_seq(First, Next, FEnv)
  end.

%% Variable references not bound in their top-level defun become ES:GLOVAR.
%% In a module they can only reference top-level defuns in the same module.
%% FEnv records those and lets us construct the required F/N fname.
translate_glovar(Var, FEnv) ->
  Arity = es_env:get(FEnv, Var),
  cerl:c_fname(Var, Arity).

translate_if(Pred, Then, Else, FEnv) ->
  %% Synthesize "case Pred of false -> Else; _ -> Then end".
  cerl:c_case(translate_expr(Pred, FEnv),
              [cerl:c_clause([cerl:c_atom('false')], translate_expr(Else, FEnv)),
               cerl:c_clause([wildpat()], translate_expr(Then, FEnv))]).

translate_lambda(Formals, Body, FEnv) ->
  assemble_lambda(translate_formals(Formals), translate_expr(Body, FEnv)).

assemble_lambda(Vars, Body) ->
  cerl:c_fun(Vars, Body).

translate_formals(Formals) ->
  lists:map(fun cerl:c_var/1, Formals).

translate_letrec(Bindings, Body, FEnv) ->
  assemble_letrec([translate_letrec_binding(Binding, FEnv) || Binding <- Bindings],
                  translate_expr(Body, FEnv)).

translate_letrec_binding({Var, Formals, Body}, FEnv) ->
  CerlFormals = translate_formals(Formals),
  Arity = length(CerlFormals),
  {cerl:c_fname(Var, Arity), CerlFormals, translate_expr(Body, FEnv)}.

assemble_letrec(Bindings, Body) ->
  FVars = [FVar || {FVar, _Formals, _Body} <- Bindings],
  cerl:c_letrec([assemble_letrec_binding(FVars, Binding) || Binding <- Bindings],
                assemble_subst(FVars, Body)).

assemble_letrec_binding(FVars, {FVar, Formals, Body}) ->
  {FVar, assemble_lambda(Formals, assemble_subst(FVars, Body))}.

assemble_subst([], Body) -> Body;
assemble_subst([FVar | FVars], Body) ->
  assemble_subst(FVars, cerl:c_let([cerl:c_var(cerl:fname_id(FVar))], FVar, Body)).

translate_let([],  Body, FEnv) ->
  translate_expr(Body, FEnv);
translate_let([{Lhs, Rhs} | Bindings], Body, FEnv) ->
  cerl:c_let([cerl:c_var(Lhs)], translate_expr(Rhs, FEnv), translate_let(Bindings, Body, FEnv)).

translate_locvar(Var) ->
  %% TODO: assumes Var is printable
  cerl:c_var(Var).

translate_primop('ES:APPLY', [Fun | Args], FEnv) ->
  CerlActuals = lists:map(fun(Arg) -> translate_expr(Arg, FEnv) end, Args),
  case Fun of
    {'ES:PRIMOP', 'ES:COLON', [M, F, {'ES:QUOTE', A}]} when A =:= length(Args) ->
      cerl:c_call(translate_expr(M, FEnv), translate_expr(F, FEnv), CerlActuals);
    _ ->
      cerl:c_apply(translate_expr(Fun, FEnv), CerlActuals)
  end;
translate_primop(PrimOp, Args0, FEnv) ->
  CerlArgs = lists:map(fun(Arg) -> translate_expr(Arg, FEnv) end, Args0),
  case {PrimOp, CerlArgs} of
    {'ES:COLON', [M, F, A]} -> make_fun(M, F, A);
    {'ES:LIST', _} -> make_list(CerlArgs);
    {'ES:RAISE', [Exn]} -> make_raise(Exn)
  end.

make_fun(M, F, A) ->
  cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('make_fun'), [M, F, A]).

make_list([]) -> cerl:c_nil();
make_list([H | T]) -> cerl:c_cons(H, make_list(T)).

make_raise(Exn) ->
  cerl:c_call(cerl:c_atom('es_datum'), cerl:c_atom('raise'), [Exn]).

translate_quote(Value) ->
  %% This is a PRE for cerl:abstract/1, but may not be true for all possible
  %% quoted values.  FIXME: how to represent other terms?
  true = cerl:is_literal_term(Value),
  cerl:abstract(Value).

translate_seq(First, Next, FEnv) ->
  cerl:c_seq(translate_expr(First, FEnv), translate_expr(Next, FEnv)).

modinfo0_def(ModuleName) ->
  {modinfo0_fname(),
   cerl:c_fun([], cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('get_module_info'), [ModuleName]))}.

modinfo0_fname() ->
  cerl:c_fname('module_info', 0).

modinfo1_def(ModuleName) ->
  Var = cerl:c_var(1),
  {modinfo1_fname(),
   cerl:c_fun([Var], cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('get_module_info'), [ModuleName, Var]))}.

modinfo1_fname() ->
  cerl:c_fname('module_info', 1).

wildpat() ->
  cerl:c_var('_').
