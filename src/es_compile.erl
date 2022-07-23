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
  FileName = binary_to_list(es_datum:string_to_binary(Arg)),
  AST = es_load:module(FileName),
  io:format("~p\n", [AST]),
  CerlModule = translate_module(AST),
  io:format("~p\n", [CerlModule]),
  {ok, _} = core_lint:module(CerlModule),
  print_module(CerlModule).

%% Internals -------------------------------------------------------------------

print_module(CerlModule) ->
  io:format("~s\n", [core_pp:format(CerlModule)]).

translate_module({'ES:MODULE', ModuleName, Exports, Defuns}) ->
  CerlModuleName = cerl:c_atom(ModuleName),
  %% Exports: add module_info/0 and module_info/1
  CerlExports = [modinfo0_fname(), modinfo1_fname() | lists:map(fun translate_export/1, Exports)],
  %% Translate each top-level (define (f ...) ...)
  %% Also define module_info/0 and module_info/1.
  CerlDefuns = [modinfo0_def(ModuleName), modinfo1_def(ModuleName) | lists:map(fun translate_defun/1, Defuns)],
  %% Assemble the module.
  cerl:c_module(CerlModuleName, CerlExports, CerlDefuns).

translate_export({F, A}) ->
  cerl:c_fname(F, A).

translate_defun({'ES:DEFINE', Var, {'ES:LAMBDA', Formals, Body}}) ->
  CerlFVar = cerl:c_fname(Var, length(Formals)),
  CerlFun = translate_lambda(Formals, Body),
  {CerlFVar, CerlFun}.

translate_expr(AST) ->
  case AST of
    {'ES:GLOVAR', Var} ->
      translate_glovar(Var);
    {'ES:IF', Pred, Then, Else} ->
      translate_if(Pred, Then, Else);
    {'ES:LAMBDA', Formals, Body} ->
      translate_lambda(Formals, Body);
    {'ES:LET', Bindings, Body} ->
      translate_let(Bindings, Body);
    {'ES:LETREC', Bindings, Body} ->
      translate_letrec(Bindings, Body);
    {'ES:LOCVAR', Var} ->
      translate_locvar(Var);
    {'ES:PRIMOP', PrimOp, Actuals} ->
      translate_primop(PrimOp, Actuals);
    {'ES:SEQ', First, Next} ->
      translate_seq(First, Next);
    {'ES:QUOTE', Value} ->
      translate_quote(Value)
  end.

translate_glovar(Var) ->
  %% Synthesize "es_gloenv:get_var(Var)".
  cerl:c_call(cerl:c_atom('es_gloenv'), cerl:c_atom('get_var'), [cerl:c_atom(Var)]).

translate_if(Pred, Then, Else) ->
  %% Synthesize "case Pred of false -> Else; _ -> Then end".
  cerl:c_case(translate_expr(Pred),
              [cerl:c_clause([cerl:c_atom('false')], translate_expr(Else)),
               cerl:c_clause([wildpat()], translate_expr(Then))]).

translate_lambda(Formals, Body) ->
  assemble_lambda(translate_formals(Formals), translate_expr(Body)).

assemble_lambda(Vars, Body) ->
  cerl:c_fun(Vars, Body).

translate_formals(Formals) ->
  lists:map(fun cerl:c_var/1, Formals).

translate_letrec(Bindings, Body) ->
  assemble_letrec([translate_letrec_binding(Binding) || Binding <- Bindings],
                  translate_expr(Body)).

translate_letrec_binding({Var, Formals, Body}) ->
  CerlFormals = translate_formals(Formals),
  Arity = length(CerlFormals),
  {cerl:c_fname(Var, Arity), CerlFormals, translate_expr(Body)}.

assemble_letrec(Bindings, Body) ->
  FVars = [FVar || {FVar, _Formals, _Body} <- Bindings],
  cerl:c_letrec([assemble_letrec_binding(FVars, Binding) || Binding <- Bindings],
                assemble_subst(FVars, Body)).

assemble_letrec_binding(FVars, {FVar, Formals, Body}) ->
  {FVar, assemble_lambda(Formals, assemble_subst(FVars, Body))}.

assemble_subst([], Body) -> Body;
assemble_subst([FVar | FVars], Body) ->
  assemble_subst(FVars, cerl:c_let([cerl:c_var(cerl:fname_id(FVar))], FVar, Body)).

translate_let([],  Body) ->
  translate_expr(Body);
translate_let([{Lhs, Rhs} | Bindings], Body) ->
  cerl:c_let([cerl:c_var(Lhs)], translate_expr(Rhs), translate_let(Bindings, Body)).

translate_locvar(Var) ->
  %% TODO: assumes Var is printable
  cerl:c_var(Var).

translate_primop('ES:APPLY', [Fun | Args]) ->
  CerlActuals = lists:map(fun translate_expr/1, Args),
  case Fun of
    {'ES:PRIMOP', 'ES:COLON', [M, F, {'ES:QUOTE', A}]} when A =:= length(Args) ->
      cerl:c_call(translate_expr(M), translate_expr(F), CerlActuals);
    _ ->
      cerl:c_apply(translate_expr(Fun), CerlActuals)
  end;
translate_primop(PrimOp, Args0) ->
  CerlArgs = lists:map(fun translate_expr/1, Args0),
  case {PrimOp, CerlArgs} of
    {'ES:COLON', [M, F, A]} -> make_fun(M, F, A);
    {'ES:LIST', _} -> make_list(CerlArgs)
  end.

make_fun(M, F, A) ->
  cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('make_fun'), [M, F, A]).

make_list([]) -> cerl:c_nil();
make_list([H | T]) -> cerl:c_cons(H, make_list(T)).

translate_seq(First, Next) ->
  cerl:c_seq(translate_expr(First), translate_expr(Next)).

translate_quote(Value) ->
  %% This is a PRE for cerl:abstract/1, but may not be true for all possible
  %% quoted values.  FIXME: how to represent other terms?
  true = cerl:is_literal_term(Value),
  cerl:abstract(Value).

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
