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

-export([init/0]).

init() ->
  define_var('*', fun '*'/2),
  define_var('+', fun '+'/2),
  define_var('eq?', fun 'eq?'/2),
  define_var('memq', fun 'memq'/2),
  define_var('symbol?', fun 'symbol?'/1),
  define_var('zero?', fun 'zero?'/1),
  define_var('null?', fun 'null?'/1),
  define_var('pair?', fun 'pair?'/1),
  define_var('list?', fun 'list?'/1),
  define_var('cons', fun 'cons'/2),
  define_var('append', fun 'append'/2),
  define_var('reverse', fun 'reverse'/1),
  define_var('car', fun 'car'/1),
  define_var('cdr', fun 'cdr'/1),
  define_var('caar', fun 'caar'/1),
  define_var('cadr', fun 'cadr'/1),
  define_var('cddr', fun 'cddr'/1),
  define_var('caddr', fun 'caddr'/1),
  define_var('cdddr', fun 'cdddr'/1),
  define_var('vector?', fun 'vector?'/1),
  define_var('eval', fun 'eval'/1),
  define_var('getprop', fun 'getprop'/2),
  define_var('putprop', fun 'putprop'/3),
  define_var('load', fun 'load'/1),
  define_var('compile', fun 'compile'/1),
  ok.

define_var(Name, Fun) ->
  es_gloenv:enter_var(Name, Fun).

'*'(X, Y) -> X * Y.

'+'(X, Y) -> X + Y.

'eq?'(X, Y) -> X =:= Y.

'memq'(X, L = [X | _]) -> L;
'memq'(X, [_ | L]) -> 'memq'(X, L);
'memq'(_, []) -> false.

'symbol?'(X) ->
  es_datum:is_symbol(X).

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

'append'(X, Y) -> X ++ Y.

'reverse'(X) ->
  lists:reverse(X).

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

'vector?'(X) ->
  es_datum:is_vector(X).

'eval'(X) -> % TODO: this should be /2 and take an environment specifier
  {Value, _SynEnv} = es_eval:eval(X, es_synenv:gloenv()),
  Value.

'getprop'(Name, Tag) ->
  case es_gloenv:lookup(Name, Tag) of
    {value, Val} -> Val;
    none -> false
  end.

'putprop'(Name, Tag, Val) ->
  es_gloenv:insert(Name, Tag, Val).

'load'(X) -> % TODO: should this take an environment specifier like eval?
  es_load:load(X, es_synenv:gloenv()).

'compile'(X) ->
  es_compile:file(X).
