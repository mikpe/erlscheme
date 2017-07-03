%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2017 Mikael Pettersson
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
%%% es_apply.erl
%%%
%%% The calling conventions of this implementation are as follows:
%%%
%%% - A fixed-arity Scheme function is represented by an Erlang function
%%%   of the same arity.  A variable-arity Scheme function is represented
%%%   by an Erlang function taking a single parameter (see below).
%%%
%%% - The caller must check that the callee's arity matches the number of
%%%   actual parameters before calling it.
%%%
%%% - If the callee's arity differs from the number of actual parameters,
%%%   it is assumed to represent a variable-arity function.  It is then
%%%   called with a list L of the actual parameters passed in a single
%%%   parameter {'$argv', L}.
%%%
%%% - A callee representing a Scheme function of arity 1 must reject being
%%%   called with an {'$argv', L} tuple, since in that case the number of
%%%   actual parameters was not 1.
%%%
%%% - A callee representing a variable-arity Scheme function must accept
%%%   either an {'$argv', L} parameter or any other parameter X, implicitly
%%%   treating the latter as {'$argv', [X]}.
%%%
%%% To avoid constructing lists of actual parameters for the common cases
%%% of matching arities, we define apply/N+1 helper functions for N <= 10.
%%% An additional applyN(F, Args) handles the general case, but requires
%%% a list of the actual parameters.
%%%
%%% A single-parameter call (F X) always becomes a direct call F(X) without
%%% additional checks, so there is no helper function for that case.

-module(es_apply).

-export([apply/1, apply/3, apply/4, apply/5, apply/6, apply/7,
         apply/8, apply/9, apply/10, apply/11, applyN/2, argv/0]).

-define(argv, '$argv').

apply(F) ->
  case erlang:fun_info(F, arity) of
    {arity, 0} -> F();
    _ -> F({?argv, []})
  end.

apply(F, A0, A1) ->
  case erlang:fun_info(F, arity) of
    {arity, 2} -> F(A0, A1);
    _ -> F({?argv, [A0, A1]})
  end.

apply(F, A0, A1, A2) ->
  case erlang:fun_info(F, arity) of
    {arity, 3} -> F(A0, A1, A2);
    _ -> F({?argv, [A0, A1, A2]})
  end.

apply(F, A0, A1, A2, A3) ->
  case erlang:fun_info(F, arity) of
    {arity, 4} -> F(A0, A1, A2, A3);
    _ -> F({?argv, [A0, A1, A2, A3]})
  end.

apply(F, A0, A1, A2, A3, A4) ->
  case erlang:fun_info(F, arity) of
    {arity, 5} -> F(A0, A1, A2, A3, A4);
    _ -> F({?argv, [A0, A1, A2, A3, A4]})
  end.

apply(F, A0, A1, A2, A3, A4, A5) ->
  case erlang:fun_info(F, arity) of
    {arity, 6} -> F(A0, A1, A2, A3, A4, A5);
    _ -> F({?argv, [A0, A1, A2, A3, A4, A5]})
  end.

apply(F, A0, A1, A2, A3, A4, A5, A6) ->
  case erlang:fun_info(F, arity) of
    {arity, 7} -> F(A0, A1, A2, A3, A4, A5, A6);
    _ -> F({?argv, [A0, A1, A2, A3, A4, A5, A6]})
  end.

apply(F, A0, A1, A2, A3, A4, A5, A6, A7) ->
  case erlang:fun_info(F, arity) of
    {arity, 8} -> F(A0, A1, A2, A3, A4, A5, A6, A7);
    _ -> F({?argv, [A0, A1, A2, A3, A4, A5, A6, A7]})
  end.

apply(F, A0, A1, A2, A3, A4, A5, A6, A7, A8) ->
  case erlang:fun_info(F, arity) of
    {arity, 9} -> F(A0, A1, A2, A3, A4, A5, A6, A7, A8);
    _ -> F({?argv, [A0, A1, A2, A3, A4, A5, A6, A7, A8]})
  end.

apply(F, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) ->
  case erlang:fun_info(F, arity) of
    {arity, 10} -> F(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9);
    _ -> F({?argv, [A0, A1, A2, A3, A4, A5, A6, A7, A8, A9]})
  end.

applyN(F, Args) ->
  N = length(Args),
  case erlang:fun_info(F, arity) of
    {arity, N} -> erlang:apply(F, Args);
    _ -> F({?argv, Args})
  end.

argv() ->
  ?argv.
