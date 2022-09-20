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
-export([ env/0
        ]).

%% Base Library functions implemented in this module
-export([ caar/1
        , cadr/1
        , caddr/1
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
   , 'car' => fun erlang:hd/1
   , 'cdr' => fun erlang:tl/1
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

%% Internals -------------------------------------------------------------------

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
  try length(X) of _ -> true catch _:_ -> false end.

'cons'(X, Y) -> [X | Y].

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
