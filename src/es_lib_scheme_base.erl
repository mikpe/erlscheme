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
%%% es_lib_scheme_base.erl
%%%
%%% Implements the Base Library (scheme base) for ErlScheme.

-module(es_lib_scheme_base).

%% API
-export([ init/0
        ]).

%% Base Library functions implemented in this module
-export([ car/1
        , caar/1
        , cadr/1
        , caddr/1
        , cdr/1
        , cddr/1
        , cdddr/1
        , cons/2
        , eval/1
        , 'list?'/1
        , load/1
        , memq/2
        , 'null?'/1
        , 'pair?'/1
        , 'zero?'/1
        ]).

%% API -------------------------------------------------------------------------

init() ->
  maps:foreach(fun es_gloenv:enter_var/2, env()).

%% Internals -------------------------------------------------------------------

env() ->
  #{ '*' => fun erlang:'*'/2
   , '+' => fun erlang:'+'/2
   , 'eq?' => fun erlang:'=:='/2
   , 'memq' => fun ?MODULE:memq/2
   , 'symbol?' => fun es_datum:is_symbol/1
   , 'zero?' => fun ?MODULE:'zero?'/1
   , 'null?' => fun ?MODULE:'null?'/1
   , 'pair?' => fun ?MODULE:'pair?'/1
   , 'list?' => fun ?MODULE:'list?'/1
   , 'cons' => fun ?MODULE:cons/2
   , 'append' => fun erlang:'++'/2
   , 'reverse' => fun lists:reverse/1
   , 'car' => fun ?MODULE:car/1
   , 'cdr' => fun ?MODULE:cdr/1
   , 'caar' => fun ?MODULE:caar/1
   , 'cadr' => fun ?MODULE:cadr/1
   , 'cddr' => fun ?MODULE:cddr/1
   , 'caddr' => fun ?MODULE:caddr/1
   , 'cdddr' => fun ?MODULE:cdddr/1
   , 'vector?' => fun erlang:is_tuple/1
   , 'eval' => fun ?MODULE:eval/1
   , 'load' => fun ?MODULE:load/1
   , 'compile' => fun es_compile:file/1
   }.

'memq'(X, L = [X | _]) -> L;
'memq'(X, [_ | L]) -> 'memq'(X, L);
'memq'(_, []) -> false.

'zero?'(X) ->
  X == 0.

'null?'(X) ->
  case X of [] -> true; _ -> false end.

'pair?'(X) ->
  case X of [_ | _] -> true; _ -> false end.

'list?'(X) ->
  listp(X). % is_list/1 is taken :-(

%% Erlang has no circular lists, so we don't need the Hare-and-Tortoise algorithm.
listp([_ | L]) -> listp(L);
listp([]) -> true;
listp(_) -> false.

'cons'(X, Y) -> [X | Y].

'car'([X | _]) ->
  X.

'cdr'([_ | Y]) ->
  Y.

'caar'([[X | _] | _]) ->
  X.

'cadr'([_, X | _]) ->
  X.

'cddr'([_, _ | Y]) ->
  Y.

'caddr'([_, _, X | _]) ->
  X.

'cdddr'([_, _, _ | Y]) ->
  Y.

'eval'(X) -> % TODO: this should be /2 and take an environment specifier
  {Value, _SynEnv} = es_eval:eval(X, es_synenv:gloenv()),
  Value.

'load'(X) -> % TODO: should this take an environment specifier like eval?
  es_load:load(X, es_synenv:gloenv()).
