%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2022 Mikael Pettersson
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
%%% es_main.erl
%%%
%%% Main entry point for ErlScheme.

-module(es_main).

-export([start/0, start/1]).

start() ->
  start([]).

start(_Argv = []) ->
  es_repl:start();
start(Argv) ->
  io:format(standard_error, "erlscheme: invalid arguments: ~p\n", [Argv]),
  halt(1).
