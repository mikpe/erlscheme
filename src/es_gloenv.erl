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
%%% es_gloenv.erl
%%%
%%% Global Name -> {Tag, Value} store for ErlScheme.
%%%
%%% Used for global variables and macros in the repl / user environment.
%%% Global variables and macros share name space so that binding an identifier
%%% as one kind simultaneously removes its binding as the other kind.

-module(es_gloenv).

-export([ enter_expander/2
        , enter_var/2
        , init/0
        , is_bound_var/1
        , lookup_expander/1
        , lookup_var/1
        ]).

-define(es_gloenv_tab, es_gloenv_tab).
-define(tag_expander, '%expander').
-define(tag_var, '%var').

-type name() :: atom().

%% API -------------------------------------------------------------------------

-spec init() -> ok.
init() ->
  ets:new(?es_gloenv_tab, [public, named_table, {read_concurrency, true}]),
  ok.

-spec enter_expander(name(), term()) -> true.
enter_expander(Name, Value) ->
  insert(Name, ?tag_expander, Value).

-spec enter_var(name(), term()) -> true.
enter_var(Name, Value) ->
  insert(Name, ?tag_var, Value).

-spec is_bound_var(name()) -> boolean().
is_bound_var(Name) ->
  case lookup(Name, ?tag_var) of
    {value, _} -> true;
    none -> false
  end.

-spec lookup_expander(name()) -> {value, term()} | none.
lookup_expander(Name) ->
  lookup(Name, ?tag_expander).

-spec lookup_var(name()) -> {value, term()} | none.
lookup_var(Name) ->
  lookup(Name, ?tag_var).

%% Internals -------------------------------------------------------------------

-spec insert(name(), ?tag_var|?tag_expander, term()) -> true.
insert(Name, Tag, Val) ->
  ets:insert(?es_gloenv_tab, {Name, {Tag, Val}}).

-spec lookup(name(), ?tag_var|?tag_expander) -> {value, term()} | none.
lookup(Name, Tag) ->
  case lookup(Name) of
    {Tag, Value} -> {value, Value};
    {_OtherTag, _Value} -> none;
    none -> none
  end.

-spec lookup(name()) -> {?tag_var|?tag_expander, term()} | none.
lookup(Name) ->
  try
    ets:lookup_element(?es_gloenv_tab, Name, 2)
  catch
    error:badarg -> none
  end.
