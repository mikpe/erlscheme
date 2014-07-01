%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014 Mikael Pettersson
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
%%% Global {Name,Tag} -> Value store for ErlScheme.
%%% Used for global variables, and macro and syntax bindings.

-module(es_gloenv).

-export([init/0,
	 destroy/0,
	 lookup/2,
	 insert/3]).

-define(es_gloenv_tab, es_gloenv_tab).
-define(es_gloenv_pid, es_gloenv_pid).

init() ->
  case ets:info(?es_gloenv_tab, type) of
    set ->
      ok;
    undefined ->
      Self = self(),
      %% XXX: monitoring here?
      P = spawn(fun () -> do_init(Self) end),
      receive {ok, P} -> ok end
  end.

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

destroy() ->
  case whereis(?es_gloenv_pid) of
    Pid when is_pid(Pid) ->
      Pid ! {destroy, self()},
      receive {ok, Pid} -> ok end;
    undefined ->
      ok
  end.

lookup(Name, Tag) ->
  try
    {value, ets:lookup_element(?es_gloenv_tab, {Name, Tag}, 2)}
  catch
    error:badarg -> none
  end.

insert(Name, Tag, Val) ->
  ets:insert(?es_gloenv_tab, {{Name, Tag}, Val}).
