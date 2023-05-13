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
%%% es_raw_port.erl
%%%
%%% A raw port is implemented by an Erlang process that maintains the
%%% port's hidden state and receives messages describing operations to
%%% perform.  A port-specific function vector determines how messages
%%% are dispatched to port-specific code.
%%%
%%% Ports transmit character data in its normal Erlang representation,
%%% in particular, no wrapped es_datum values occur, characters are
%%% non-negative integers, and EOF is -1.
%%%
%%% TODO:
%%% - textual-port stuff
%%% - binary-port stuff

-module(es_raw_port).
-behaviour(gen_server).

%% API
-export([ close_input_port/1
        , format_error/1
        , open_input_file/1
        , open_input_string/1
        , open_stdin/0
        , read_char/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export_type([ es_port/0
             ]).

-type es_port() :: pid().

%% commands
-define(close, close).
-define(file, file).
-define(read_char, read_char).
-define(stdin, stdin).
-define(string, string).

%% API -------------------------------------------------------------------------

-spec close_input_port(es_port()) -> ok.
close_input_port(Pid) ->
  call(Pid, ?close).

-spec open_input_file(file:filename_all()) -> es_port().
open_input_file(Path) ->
  open_input({?file, Path}).

-spec open_input_string(string()) -> es_port().
open_input_string(String) ->
  open_input({?string, String}).

-spec open_stdin() -> es_port().
open_stdin() ->
  open_input(?stdin).

-spec read_char(es_port()) -> integer().
read_char(Pid) ->
  call(Pid, ?read_char).

%% API Internals ---------------------------------------------------------------

call(Pid, Cmd) ->
  case gen_server:call(Pid, Cmd, 'infinity') of
    {ok, Res} -> Res;
    {error, Reason} -> error(Reason)
  end.

open_input(Arg) ->
  case gen_server:start(?MODULE, Arg, []) of
    {ok, Pid} -> Pid;
    {error, {shutdown, Reason}} -> error(Reason)
  end.

%% gen_server callbacks --------------------------------------------------------

-record(input_port_funs,
        { close
        , read_char
        }).

-record(server_state,
        { funs :: #input_port_funs{}
        , state :: any()
        }).

init(Arg) ->
  InitRes =
    case Arg of
      {?file, Path} -> do_open_input_file(Path);
      {?string, String} -> do_open_input_string(String);
      ?stdin -> do_open_stdin()
    end,
  case InitRes of
    {ok, _ServerState = #server_state{}} ->
      InitRes;
    {error, Reason} ->
      %% The {shutdown, ...} wrapper prevents an unwanted crash report.
      {stop, {shutdown, Reason}}
  end.

handle_call(Req, _From, State) ->
  case Req of
    ?close ->
      Result = handle_close(State),
      {stop, normal, Result, []};
    ?read_char ->
      {Result, NewState} = handle_read_char(State),
      {reply, Result, NewState};
    _ ->
      {reply, {error, {?MODULE, {bad_call, Req}}}, State}
  end.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State = []) -> % terminating due to explicit close
  ok;
terminate(_Reason, State) ->
  _ = handle_close(State),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% gen_server internals --------------------------------------------------------

handle_close(#server_state{funs = Funs, state = State}) ->
  (Funs#input_port_funs.close)(State).

handle_read_char(ServerState = #server_state{funs = Funs, state = State}) ->
  {Result, NewState} = (Funs#input_port_funs.read_char)(State),
  {Result, ServerState#server_state{state = NewState}}.

%% String input ports ----------------------------------------------------------

do_open_input_string(String) ->
  Funs =
    #input_port_funs
      { close = fun noop_close/1
      , read_char = fun input_string_read_char/1
      },
  {ok, #server_state{funs = Funs, state = String}}.

noop_close(_) ->
  {ok, true}.

input_string_read_char([C | State]) ->
  {{ok, C}, State};
input_string_read_char([] = _State) ->
  {{ok, -1}, []}.

%% File input ports ------------------------------------------------------------

do_open_input_file(Path) ->
  %% We MUST call file:open/2 from within the gen_server.
  %% Note: This only works for text files, e.g. source code.
  case file:open(Path, [read, {encoding, utf8}, read_ahead]) of
    {ok, IoDev} ->
      Funs =
        #input_port_funs
          { close = fun input_file_close/1
          , read_char = fun input_file_read_char/1
          },
      {ok, #server_state{funs = Funs, state = {[], IoDev}}};
    {error, Reason} ->
      {error, {?MODULE, {bad_file, Path, Reason}}}
  end.

input_file_close({_, IoDev}) ->
  case file:close(IoDev) of
    ok -> {ok, true};
    {error, Reason} -> {error, {file, Reason}}
  end.

input_file_read_char({Buf, IoDev}) ->
  case Buf of
    [] ->
      case io:get_line(IoDev, []) of
        [Ch | Rest] ->
          {{ok, Ch}, {Rest, IoDev}};
        eof ->
         {{ok, -1}, {[], IoDev}}
      end;
    [Ch | Rest] ->
      {{ok, Ch}, {Rest, IoDev}}
  end.

%% Standard input port ---------------------------------------------------------

do_open_stdin() ->
  Funs = #input_port_funs
    { close = fun noop_close/1
    , read_char = fun input_file_read_char/1
    },
  {ok, #server_state{funs = Funs, state = {[], standard_io}}}.

%% Error Formatting ------------------------------------------------------------

-spec format_error(term()) -> io_lib:chars().
format_error(Reason) ->
  case Reason of
    {bad_call, X} ->
      io_lib:format("invalid call: ~tp", [X]);
    {bad_file, Path, Reason2} ->
      io_lib:format("unable to open ~ts: ~ts", [Path, file:format_error(Reason2)]);
    _ ->
      io_lib:format("~tp", [Reason])
  end.
