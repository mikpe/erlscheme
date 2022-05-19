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
%%% es_repl.erl
%%%
%%% Read-Eval-Print-Loop for ErlScheme.

-module(es_repl).

-export([start/0]).

start() ->
  case init() of
    false -> false;
    LI -> repl(1, LI)
  end.

init() ->
  try
    io:format("Welcome to ErlScheme version ~s~n", [?VSN]),
    es_gloenv:init(),
    es_lib_scheme_base:init(),
    es_load_init(),
    P = es_raw_port:open_stdin(),
    es_lexinput:open(P, "<stdin>")
  catch
    Class:Reason:Stack ->
      io:format("ErlScheme: fatal error ~p:~p during startup~n~p~n",
		[Class, Reason, Stack]),
      false
  end.

es_load_init() ->
  io:format("Loading es_init.scm ..."),
  {ok, ScmPrefix} = es_path:lib_dir("scm"),
  erlang:put('es_load_prefix', ScmPrefix),
  es_load:load(es_datum:binary_to_string(<<"es-init.scm">>)),
  io:format(" done~n").

repl(N, LI) ->
  try
    io:format("ErlScheme_~p> ", [N]),
    Sexpr = es_read:read(LI),
    erlang:put('es_load_prefix', "."),
    Term = es_eval:dynamic_eval(Sexpr),
    es_print:display(Term),
    io:format("~n")
  catch
    Class:Reason:Stack ->
      io:format("caught ~p:~p~n~p~n", [Class, Reason, Stack])
  end,
  repl(N + 1, LI).
