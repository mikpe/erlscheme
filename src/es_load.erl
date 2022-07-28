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
%%% es_load.erl
%%%
%%% Loads S-expressions from file.

-module(es_load).

-export([ load/2
        , module/1
        ]).

-type ast() :: term().
-type datum() :: term().
-type synenv() :: es_macros:synenv().

%% API -------------------------------------------------------------------------

-spec load(datum(), synenv()) -> synenv().
load(Name, SynEnv) ->
  case es_datum:is_string(Name) of
    true -> % (load "Name.scm")
      load_file(Name, SynEnv);
    false -> % (load 'Name)
      true = es_datum:is_symbol(Name),
      load_module(Name),
      SynEnv
  end.

-spec module(string()) -> ast().
module(FileName) ->
  SynEnv =
    lists:foldl(
      fun ({Name, Expander}, SynEnv0) ->
        es_synenv:enter(SynEnv0, Name, Expander)
      end, es_synenv:empty(), es_macros:initial()),
  {RevSexprs, _SynEnv} = load(fun do_expand/2, {[], SynEnv}, FileName),
  Sexprs = lists:reverse(RevSexprs),
  es_parse:module(Sexprs).

%% Internals -------------------------------------------------------------------

load_module(Name) ->
  code:is_loaded(Name) orelse begin {module, _} = code:load_file(Name), true end.

load_file(String, SynEnv) ->
  FileName = binary_to_list(es_datum:string_to_binary(String)),
  load(fun do_eval/2, SynEnv, FileName).

do_eval(Datum, SynEnv) ->
  {_Result, NewSynEnv} = es_eval:eval(Datum, SynEnv),
  NewSynEnv.

do_expand(Datum, {Acc, SynEnv}) ->
  {Expanded, NewSynEnv} = es_macros:expand_toplevel(Datum, SynEnv),
  {[Expanded | Acc], NewSynEnv}.

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
