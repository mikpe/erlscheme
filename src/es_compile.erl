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

-export([file/1]).

%%% TODO: extend es_parse to recognize (module Module (Function*))?
%%% TODO: perhaps extend es_load to handle the module form, would
%%%       require persistent local scopes for global variables?

file(Arg) ->
  FileName = binary_to_list(es_datum:string_to_binary(Arg)),
  AST = es_load:module(FileName),
  io:format("~p\n", [AST]),
  CerlModule = translate_module(FileName, AST),
  io:format("~p\n", [CerlModule]),
  {ok, _} = core_lint:module(CerlModule),
  print_module(CerlModule).

print_module(CerlModule) ->
  io:format("~s\n", [core_pp:format(CerlModule)]).

translate_module(FileName, ASTs) ->
  %% Module name: derived from FileName (foo.scm -> foo).
  ModuleName = cerl:c_atom(filename:basename(FileName, ".scm")),
  %% Exports: ['$es_init/0', module_info/0, module_info/1]
  FVarInit = cerl:c_fname('$es_init', 0),
  Exports = [FVarInit, modinfo0_fname(), modinfo1_fname()],
  %% Definitions: FVarInit = fun() -> Body end.
  %% where the Body evaluates the ASTs and records each
  %% top-level (define ...) as a global variable.
  %% Also define module_info/0 and module_info/1.
  Body = translate_toplevel(ASTs),
  FunInit = cerl:c_fun([], Body),
  Definitions = [{FVarInit, FunInit}, modinfo0_def(ModuleName), modinfo1_def(ModuleName)],
  %% Assemble the module.
  cerl:c_module(ModuleName, Exports, Definitions).

translate_toplevel(ASTs) ->
  Cerls = [translate_expr(AST, true) || AST <- ASTs],
  make_seq(Cerls).

make_seq([]) -> cerl:c_nil(); % #!undefined
make_seq([H | T]) -> make_seq(H, T).

make_seq(X, []) -> X;
make_seq(X, [Y | Z]) -> cerl:c_seq(X, make_seq(Y, Z)).

translate_expr(AST) ->
  translate_expr(AST, false).

translate_expr(AST, IsToplevel) ->
  case AST of
    {'ES:CALL', Fun, Actuals} ->
      translate_call(Fun, Actuals);
    {'ES:DEFINE', Var, Expr} when IsToplevel ->
      translate_define(Var, Expr);
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
    {'ES:SEQ', First, Next} ->
      translate_seq(First, Next, IsToplevel);
    {'ES:QUOTE', Value} ->
      translate_quote(Value)
  end.

translate_call(Fun, Actuals) ->
  CerlFun = translate_expr(Fun),
  CerlActuals = [translate_expr(Actual) || Actual <- Actuals],
  case length(Actuals) of
    1 ->
      %% Neither cerl, core_lint, nor core_pp reject c_apply:s with non-variables
      %% in their function position, but the BEAM compiler (sys_core_fold) does.
      Tmp = newvar(),
      cerl:c_let([Tmp], CerlFun, cerl:c_apply(Tmp, CerlActuals));
    N when N =< 10 ->
      cerl:c_call(cerl:c_atom('es_apply'), cerl:c_atom('apply'),
		  [CerlFun | CerlActuals]);
    _ ->
      ListOfActuals = make_list(CerlActuals),
      cerl:c_call(cerl:c_atom('es_apply'), cerl:c_atom('applyN'),
		  [CerlFun, ListOfActuals])
  end.

make_list([]) -> cerl:c_nil();
make_list([H | T]) -> cerl:c_cons(H, make_list(T)).

translate_define(Var, Expr) ->
  %% Synthesize `es_gloenv:enter_var(Var, Expr)'.
  cerl:c_call(cerl:c_atom('es_gloenv'), cerl:c_atom('enter_var'),
	      [cerl:c_atom(Var), translate_expr(Expr)]).

translate_glovar(Var) ->
  %% Synthesize `es_gloenv:get_var(Var)'.
  cerl:c_call(cerl:c_atom('es_gloenv'), cerl:c_atom('get_var'), [cerl:c_atom(Var)]).

translate_if(Pred, Then, Else) ->
  %% Synthesize `case Pred of false -> Else; _ -> Then end'.
  cerl:c_case(translate_expr(Pred),
	      [cerl:c_clause([cerl:c_atom('false')], translate_expr(Else)),
	       cerl:c_clause([wildpat()], translate_expr(Then))]).

translate_lambda(Formals, Body) ->
  assemble_lambda(translate_formals(Formals), translate_expr(Body)).

assemble_lambda(Formals, Body) ->
  case Formals of
    {variadic, ListPattern} -> assemble_lambda_varargs(ListPattern, Body);
    {fixed, [Var]} -> assemble_lambda_1(Var, Body);
    {fixed, Vars} -> assemble_lambda_N(Vars, Body)
  end.

assemble_lambda_varargs(ListPattern, Body) -> % accept vararg or non-vararg parameter
  %% Synthesize:
  %% fun (_1) ->
  %%   case (case _1 of {'$argv', _2} -> _2; _ -> [_1] end) of
  %%     ListPattern -> Body;
  %%     _ -> erlang:error(badarity)
  %%   end.
  Var1 = cerl:c_var(1), % '_1'
  Var2 = cerl:c_var(2), % '_2'
  cerl:c_fun([Var1],
	     cerl:c_case(cerl:c_case(Var1,
				     [cerl:c_clause([cerl:c_tuple_skel([cerl:c_atom('$argv'), Var2])], Var2),
				      cerl:c_clause([wildpat()], cerl:c_cons(Var1, cerl:c_nil()))]),
			 [cerl:c_clause([ListPattern], Body),
			  cerl:c_clause([wildpat()],
					cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('error'), [cerl:c_atom('badarity')]))])).

assemble_lambda_1(Var, Body) -> % an arity-1 lambda must reject a varargs parameter
  %% Synthesize:
  %% fun (Var) -> case Var of {'$argv', _} -> erlang:error(badarity); _ -> Body end
  cerl:c_fun([Var],
	     cerl:c_case(Var,
			 [cerl:c_clause([cerl:c_tuple_skel([cerl:c_atom('$argv'), wildpat()])],
					cerl:c_call(cerl:c_atom('erlang'), cerl:c_atom('error'), [cerl:c_atom('badarity')])),
			  cerl:c_clause([wildpat()], Body)])).

assemble_lambda_N(Vars, Body) -> % any fixed arity except 1
  cerl:c_fun(Vars, Body).

translate_formals(Formals) -> translate_formals(Formals, []).

translate_formals([], Acc) -> {fixed, lists:reverse(Acc)};
translate_formals([Var | Formals], Acc) -> translate_formals(Formals, [cerl:c_var(Var) | Acc]);
translate_formals(Var, Acc) when is_atom(Var) -> {variadic, make_varargs_pattern(cerl:c_var(Var), Acc)}.

make_varargs_pattern(Rest, []) -> Rest;
make_varargs_pattern(Rest, [T | H]) ->
  make_varargs_pattern(cerl:c_cons_skel(T, Rest), H).

translate_letrec(Bindings, Body) ->
  assemble_letrec([translate_letrec_binding(Binding) || Binding <- Bindings],
		  translate_expr(Body)).

translate_letrec_binding({Var, Formals, Body}) ->
  CerlFormals = translate_formals(Formals),
  Arity =
    case CerlFormals of
      {variadic, _ListPattern} -> 1;
      {fixed, Vars} -> length(Vars)
    end,
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

translate_seq(First, Next, IsToplevel) ->
  make_seq(translate_expr(First, IsToplevel), translate_expr(Next, IsToplevel)).

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

newvar() ->
  %% Neither cerl, core_lint, nor core_pp reject negative numeric variable names,
  %% but the BEAM compiler throws syntax errors on .core files containing them.
  cerl:c_var(erlang:unique_integer([positive])).
