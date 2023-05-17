%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2023 Mikael Pettersson
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
%%% es_input_string_iodev.erl
%%%
%%% An Erlang I/O protocol server that implements sequential single-character
%%% reads from strings.
%%%
%%% The I/O server is required to convert from sequences of octets to Unicode
%%% characters. This needs a fair amount of code, so we support reading from
%%% binaries by converting them to strings first.
%%%
%%% See https://www.erlang.org/doc/apps/stdlib/io_protocol.html for details
%%% about the Erlang I/O protocol.

-module(es_input_string_iodev).
-behaviour(gen_server).

%% API
-export([ close/1
        , open/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(close, close).

%% API -------------------------------------------------------------------------

-spec close(pid()) -> ok.
close(Pid) ->
  ok = gen_server:call(Pid, ?close, infinity).

-spec open(string() | binary()) -> pid().
open(Binary) when is_binary(Binary) -> open(unicode:characters_to_list(Binary));
open(String) when is_list(String) ->
  {ok, Pid} = gen_server:start_link(?MODULE, String, []),
  Pid.

%% gen_server callbacks --------------------------------------------------------

-type state() :: string().

init(String) ->
  {ok, String}.

handle_call(Req, _From, State) ->
  case Req of
    ?close ->
      {stop, normal, ok, []};
    _ ->
      {reply, {error, {bad_call, Req}}, State}
  end.

handle_cast(_Req, State) ->
  {noreply, State}.

%% All I/O commands come in here.
handle_info(Info, State) ->
  %% io:format(standard_error, ?MODULE_STRING ": handle_info: info=~p state=~p\n", [Info, State]),
  case Info of
    {io_request, From, ReplyAs, Request} ->
      {Reply, NewState} =
        try io_request(Request, State)
        catch _Class:Reason:_ST ->
          %% io:format(standard_error, ?MODULE_STRING ": crashed with ~p:~p\n~p\n", [_Class, Reason, _ST]),
          {{error, Reason}, State}
        end,
      %% io:format(standard_error, ?MODULE_STRING ": reply ~p to ~p\n", [Reply, From]),
      io_reply(From, ReplyAs, Reply),
      {noreply, NewState};
    _ ->
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% I/O server specifics --------------------------------------------------------

-type io_request() :: term().
-type io_result() :: string() | eof | {error, term()}.

-spec io_reply(pid(), term(), io_result()) -> any().
io_reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

-spec io_request(io_request(), state()) -> {io_result(), state()}.
io_request(Request, State) ->
  case Request of
    %% This is enough to support io:get_chars(StringDev, "", 1).
    {get_chars, unicode, _Prompt, 1} -> get_char(State);
    %% We return {error, enotsup} for unimplemented commands and {error, request}
    %% for unrecognized commands, as per recommendations in the documentation.
    {get_chars, _Encoding, _Prompt, _N} -> error_enotsup(State);
    {get_until, _Encoding, _Prompt, _Mod, _Func, _Args} -> error_enotsup(State);
    {get_line, _Encoding, _Prompt} -> error_enotsup(State);
    {setopts, _Opts} -> error_enotsup(State);
    getopts -> error_enotsup(State);
    {requests, _Requests} -> error_enotsup(State);
    {get_geometry, _Geometry} -> error_enotsup(State);
    {put_chars, _Encoding, _Chars} -> error_enotsup(State);
    {put_chars, _Encoding, _Module, _Function, _Args} -> error_enotsup(State);
    _ -> error_request(State)
  end.

get_char(String) ->
  case String of
    [Ch | Rest] -> {[Ch], Rest};
    [] -> {eof, []}
  end.

error_enotsup(State) ->
  {{error, enotsup}, State}.

error_request(State) ->
  {{error, request}, State}.
