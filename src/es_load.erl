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
%%% es_load.erl
%%%
%%% Loads S-expressions from file.

-module(es_load).

-export([load/1, expand_and_parse/1]).

load(FileName) ->
  load(fun do_eval/2, [], FileName),
  true.

do_eval(Datum, _Acc) ->
  es_eval:dynamic_eval(Datum).

expand_and_parse(FileName) ->
  lists:reverse(load(fun do_expand_and_parse/2, [], FileName)).

do_expand_and_parse(Datum, Acc) ->
  Fun = es_gloenv:get_var('%expand-macros'),
  Sexpr = es_eval:do_apply(Fun, [Datum]),
  [es_parse:toplevel(Sexpr) | Acc].

load(Fun, Acc, FileName) ->
  OldPrefix = erlang:get('es_load_prefix'),
  NewPath = filename:join(OldPrefix, FileName),
  P = es_raw_port:open_input_file(NewPath),
  erlang:put('es_load_prefix', filename:dirname(NewPath)),
  try
    LI = es_lexinput:open(P, FileName),
    try
      loop(Fun, Acc, LI)
    after
      es_lexinput:close(LI)
    end
  after
    erlang:put('es_load_prefix', OldPrefix)
  end.

loop(Fun, Acc, LI) ->
  Datum = es_read:read(LI),
  case es_datum:is_eof_object(Datum) of
    false ->
      loop(Fun, Fun(Datum, Acc), LI);
    true ->
      Acc
  end.
