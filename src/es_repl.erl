%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014-2023 Mikael Pettersson
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
    io:format("Welcome to ErlScheme version ~ts\n", [?VSN]),
    io:format("~ts\n", [erlang:system_info(system_version)]),
    es_gloenv:init(),
    es_lib_scheme_base_init(),
    es_macros_init(),
    es_load_init(),
    es_lexinput:open_stdin()
  catch
    Class:Reason:Stack ->
      io:format("fatal ~tp during startup: ~ts\n", [Class, es_error:format(Reason)]),
      io:format("stack trace:\n~tp\n", [Stack]),
      false
  end.

es_lib_scheme_base_init() ->
  maps_foreach(fun es_gloenv:enter_var/2, es_lib_scheme_base:env()).

%% TODO: remove this when we no longer support OTP-23
-if(?OTP_RELEASE >= 24).
maps_foreach(Fun, Map) ->
  maps:foreach(Fun, Map).
-else.
maps_foreach(Fun, Map) ->
  maps:fold(fun(K, V, _Acc) -> Fun(K, V), ok end, ok, Map).
-endif.

es_macros_init() ->
  lists:foreach(
    fun ({Name, Expander}) ->
      es_gloenv:enter_expander(Name, Expander)
    end, es_macros:initial()).

es_load_init() ->
  io:format("Loading es_init.scm ..."),
  PrivDir = code:priv_dir(erlscheme),
  ScmPrefix = filename:join(PrivDir, "scm"),
  true = filelib:is_dir(ScmPrefix),
  erlang:put('es_load_prefix', ScmPrefix),
  es_load:load(<<"es-init.scm">>, es_synenv:gloenv()),
  io:format(" done\n\n").

repl(N, LI) ->
  case rep(N, LI) of
    ok -> repl(N + 1, LI);
    false -> ok
  end.

rep(N, LI) ->
  try
    io:format("ErlScheme_~tp> ", [N]),
    Sexpr = es_read:read(LI),
    case es_datum:is_eof_object(Sexpr) of
      false ->
        erlang:put('es_load_prefix', "."),
        {Term, _SynEnv} = es_eval:eval(Sexpr, es_synenv:gloenv()),
        es_print:display(Term),
        io:format("\n");
      true ->
        false
    end
  catch
    Class:Reason:Stack ->
      io:format("caught ~tp: ~ts\n", [Class, format_exn(Class, Reason)]),
      io:format("stack trace:\n~tp\n", [Stack])
  end.

format_exn(error, Reason) -> es_error:format(Reason);
format_exn(_, Reason) -> io_lib:format("~tp", [Reason]).
