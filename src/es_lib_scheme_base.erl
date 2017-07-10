%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014-2017 Mikael Pettersson
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
  define_var(':', fun ':'/2), % ErlScheme-specific hook into Erlang
  define_var('*', fun '*'/1), % varargs
  define_var('+', fun '+'/1), % varargs
  define_var('eq?', fun 'eq?'/2),
  define_var('memq', fun 'memq'/2),
  define_var('symbol?', fun 'symbol?'/1),
  define_var('zero?', fun 'zero?'/1),
  define_var('null?', fun 'null?'/1),
  define_var('pair?', fun 'pair?'/1),
  define_var('list?', fun 'list?'/1),
  define_var('list', fun 'list'/1), % varargs
  define_var('cons', fun 'cons'/2),
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
  ok.

define_var(Name, Fun) ->
  es_gloenv:enter_var(Name, Fun).

':'(M, F) ->
  fun (Arg) -> % varargs
      erlang:apply(M, F, get_varargs(Arg))
  end.

'*'(Arg) -> '*'(get_varargs(Arg), 1).
'*'([], Acc) -> Acc;
'*'([H | T], Acc) -> '*'(T, Acc * H).

'+'(Arg) -> '+'(get_varargs(Arg), 0).
'+'([], Acc) -> Acc;
'+'([H | T], Acc) -> '+'(T, Acc + H).

'eq?'(X, Y) -> X =:= Y.

'memq'(X, L = [X | _]) -> L;
'memq'(X, [_ | L]) -> 'memq'(X, L);
'memq'(_, []) -> false.

'symbol?'(Arg) ->
  X = get_onearg(Arg),
  es_datum:is_symbol(X).

'zero?'(Arg) ->
  X = get_onearg(Arg),
  X == 0.

'null?'(Arg) ->
  X = get_onearg(Arg),
  case X of [] -> true; _ -> false end.

'pair?'(Arg) ->
  X = get_onearg(Arg),
  case X of [_ | _] -> true; _ -> false end.

'list?'(Arg) ->
  X = get_onearg(Arg),
  listp(X). % is_list/1 is taken :-(

%% Erlang has no circular lists, so we don't need the Hare-and-Tortoise algorithm.
listp([_ | L]) -> listp(L);
listp([]) -> true;
listp(_) -> false.

'list'(Arg) -> get_varargs(Arg).

'cons'(X, Y) -> [X | Y].

'car'(Arg) ->
  [X | _] = get_onearg(Arg),
  X.

'cdr'(Arg) ->
  [_ | Y] = get_onearg(Arg),
  Y.

'caar'(Arg) ->
  [[X | _] | _] = get_onearg(Arg),
  X.

'cadr'(Arg) ->
  [_, X | _] = get_onearg(Arg),
  X.

'cddr'(Arg) ->
  [_, _ | Y] = get_onearg(Arg),
  Y.

'caddr'(Arg) ->
  [_, _, X | _] = get_onearg(Arg),
  X.

'cdddr'(Arg) ->
  [_, _, _ | Y] = get_onearg(Arg),
  Y.

'vector?'(Arg) ->
  X = get_onearg(Arg),
  es_datum:is_vector(X).

'eval'(Arg) ->
  X = get_onearg(Arg),
  es_eval:primitive_eval(X).

'getprop'(Name, Tag) ->
  case es_gloenv:lookup(Name, Tag) of
    {value, Val} -> Val;
    none -> false
  end.

'putprop'(Name, Tag, Val) ->
  es_gloenv:insert(Name, Tag, Val).

'load'(Arg) ->
  String = get_onearg(Arg),
  es_load:load(binary_to_list(es_datum:string_to_binary(String))).

%% Parameter parsing helpers

-define(argv, '$argv').

get_onearg({?argv, _L}) -> error(badarity);
get_onearg(X) -> X.

get_varargs({?argv, L}) -> L;
get_varargs(X) -> [X].
