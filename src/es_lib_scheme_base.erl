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

-export([init/0, do_load/1]).

init() ->
  define_var(':', fun ':'/1), % ErlScheme-specific hook into Erlang
  define_var('*', fun '*'/1),
  define_var('+', fun '+'/1),
  define_var('eq?', fun 'eq?'/1),
  define_var('memq', fun 'memq'/1),
  define_var('symbol?', fun 'symbol?'/1),
  define_var('zero?', fun 'zero?'/1),
  define_var('null?', fun 'null?'/1),
  define_var('pair?', fun 'pair?'/1),
  define_var('list?', fun 'list?'/1),
  define_var('list', fun 'list'/1),
  define_var('cons', fun 'cons'/1),
  define_var('car', fun 'car'/1),
  define_var('cdr', fun 'cdr'/1),
  define_var('caar', fun 'caar'/1),
  define_var('cadr', fun 'cadr'/1),
  define_var('cddr', fun 'cddr'/1),
  define_var('caddr', fun 'caddr'/1),
  define_var('cdddr', fun 'cdddr'/1),
  define_var('vector?', fun 'vector?'/1),
  define_var('eval', fun 'eval'/1),
  define_var('getprop', fun 'getprop'/1),
  define_var('putprop', fun 'putprop'/1),
  define_var('load', fun 'load'/1),
  ok.

define_var(Name, Fun) ->
  es_gloenv:insert(Name, 'var', Fun).

':'([M, F]) ->
  fun (Args) ->
      erlang:apply(M, F, Args)
  end.

'*'(Args) -> '*'(Args, 1).
'*'([], Acc) -> Acc;
'*'([H | T], Acc) -> '*'(T, Acc * H).

'+'(Args) -> '+'(Args, 0).
'+'([], Acc) -> Acc;
'+'([H | T], Acc) -> '+'(T, Acc + H).

'eq?'([X, Y]) -> X =:= Y.

'memq'([X, L]) -> 'memq'(X, L).
'memq'(X, L = [X | _]) -> L;
'memq'(X, [_ | L]) -> 'memq'(X, L);
'memq'(_, []) -> false.

'symbol?'([X]) -> es_datum:is_symbol(X).

'zero?'([X]) -> X == 0.

'null?'([X]) -> case X of [] -> true; _ -> false end.

'pair?'([X]) -> case X of [_ | _] -> true; _ -> false end.

'list?'([X]) -> listp(X). % is_list/1 is taken :-(
%% Erlang has no circular lists, so we don't need the Hare-and-Tortoise algorithm.
listp([_ | L]) -> listp(L);
listp([]) -> true;
listp(_) -> false.

'list'(Args) -> Args.

'cons'([X, Y]) -> [X | Y].

'car'  ([[X | _]]) -> X.
'cdr'  ([[_ | Y]]) -> Y.
'caar' ([[[X | _] | _]]) -> X.
'cadr' ([[_ , X | _]]) -> X.
'cddr' ([[_ , _ | Y]]) -> Y.
'caddr'([[_ , _ , X | _]]) -> X.
'cdddr'([[_ , _ , _ | Y]]) -> Y.

'vector?'([X]) -> es_datum:is_vector(X).

'eval'([X]) -> es_eval:primitive_eval(X).

'getprop'([Name, Tag]) ->
  case es_gloenv:lookup(Name, Tag) of
    {value, Val} -> Val;
    none -> false
  end.

'putprop'([Name, Tag, Val]) ->
  es_gloenv:insert(Name, Tag, Val).

'load'([String]) ->
  do_load(binary_to_list(es_datum:string_to_binary(String))).

do_load(FileName) ->
  OldPrefix = erlang:get('es_load_prefix'),
  NewPath = filename:join(OldPrefix, FileName),
  P = es_raw_port:open_input_file(NewPath),
  erlang:put('es_load_prefix', filename:dirname(NewPath)),
  try
    LI = es_lexinput:open(P, FileName),
    try
      load_loop(LI)
    after
      es_lexinput:close(LI)
    end
  after
    erlang:put('es_load_prefix', OldPrefix)
  end.

load_loop(LI) ->
  Datum = es_read:read(LI),
  case es_datum:is_eof_object(Datum) of
    false ->
      es_eval:dynamic_eval(Datum),
      load_loop(LI);
    true ->
      true
  end.
