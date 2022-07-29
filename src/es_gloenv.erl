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

-export([ destroy/0
        , enter_var/2
        , get_var/1
        , init/0
        , insert/3
        , lookup/2
        ]).

-define(es_gloenv_tab, es_gloenv_tab).
-define(es_gloenv_pid, es_gloenv_pid).
-define(tag_var, '%var').

%% API -------------------------------------------------------------------------

-spec init() -> ok.
init() ->
  case ets:info(?es_gloenv_tab, type) of
    set ->
      ok;
    undefined ->
      Self = self(),
      %% FIXME: monitoring here?
      P = spawn(fun () -> do_init(Self) end),
      receive {ok, P} -> ok end
  end.

-spec destroy() -> ok.
destroy() ->
  case whereis(?es_gloenv_pid) of
    Pid when is_pid(Pid) ->
      Pid ! {destroy, self()},
      receive {ok, Pid} -> ok end;
    undefined ->
      ok
  end.

-spec get_var(term()) -> term().
get_var(Name) ->
  case lookup(Name, ?tag_var) of
    {value, Value} -> Value;
    none -> throw({unbound_variable, Name})
  end.

-spec enter_var(term(), term()) -> true.
enter_var(Name, Value) -> insert(Name, ?tag_var, Value).

-spec lookup(term(), atom()) -> {value, term()} | none.
lookup(Name, Tag) ->
  case lookup(Name) of
    {Tag, Value} -> {value, Value};
    {_OtherTag, _Value} -> none;
    none -> none
  end.

-spec insert(term(), atom(), term()) -> true.
insert(Name, Tag, Val) ->
  ets:insert(?es_gloenv_tab, {Name, {Tag, Val}}).

%% Internals -------------------------------------------------------------------

do_init(Pid) ->
  erlang:register(?es_gloenv_pid, self()),
  ets:new(?es_gloenv_tab, [public, named_table, {read_concurrency, true}]),
  Pid ! {ok, self()},
  wait_for_destroy().

wait_for_destroy() ->
  receive
    {destroy, Pid} ->
      Pid ! {ok, self()};
    _ ->
      wait_for_destroy()
  end.

-spec lookup(term()) -> {atom(), term()} | none.
lookup(Name) ->
  try
    ets:lookup_element(?es_gloenv_tab, Name, 2)
  catch
    error:badarg -> none
  end.
