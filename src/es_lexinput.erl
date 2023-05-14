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
%%% es_lexinput.erl
%%%
%%% Wraps an input port with a gen_server to maintain line number and column,
%%% the latest peeked character (if any), and its file name (if any).

-module(es_lexinput).
-behaviour(gen_server).

%% API
-export([ close/1
        , column/1
        , line/1
        , name/1
        , open_file/1
        , open_stdin/0
        , open_string/1
        , peek_char/1
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

-export_type([ lexinput/0
             ]).

-type lexinput() :: pid().

%% commands
-define(close, close).
-define(column, column).
-define(file, file).
-define(line, line).
-define(name, name).
-define(peek_char, peek_char).
-define(read_char, read_char).
-define(stdin, stdin).
-define(string, string).

%% API -------------------------------------------------------------------------

-spec close(lexinput()) -> ok.
close(LI) ->
  call(LI, ?close).

-spec column(lexinput()) -> integer().
column(LI) ->
  call(LI, ?column).

-spec line(lexinput()) -> integer().
line(LI) ->
  call(LI, ?line).

-spec name(lexinput()) -> file:filename_all().
name(LI) ->
  call(LI, ?name).

-spec open_file(file:filename_all()) -> lexinput().
open_file(Path) ->
  open({?file, Path}).

-spec open_stdin() -> lexinput().
open_stdin() ->
  open(?stdin).

-spec open_string(string()) -> lexinput().
open_string(String) ->
  open({?string, String}).

-spec peek_char(lexinput()) -> integer().
peek_char(LI) ->
  call(LI, ?peek_char).

-spec read_char(lexinput()) -> integer().
read_char(LI) ->
  call(LI, ?read_char).

%% API Internals ---------------------------------------------------------------

call(Pid, Cmd) ->
  %% deliberately throw in case of error
  {ok, Res} = gen_server:call(Pid, Cmd, 'infinity'),
  Res.

open(Arg) ->
  case gen_server:start(?MODULE, Arg, []) of
    {ok, Pid} -> Pid;
    {error, {shutdown, Reason}} -> error(Reason)
  end.

%% gen_server callbacks --------------------------------------------------------

-record(state,
        { %% name and port access functions don't change after init
          name
        , port_read_char
        , port_close
          %% port-specific state (IoDev handle or string buffer)
        , port_state
          %% if peeked is =/= [] it is the value of the last retrieved character,
          %% which was peeked not read, and line and column have not been updated
        , peeked
          %% line and column give the position of the next character to be retrieved
        , line
        , column
        }).

init(Arg) ->
  case handle_init(Arg) of
    {ok, {Name, PortReadChar, PortClose, PortState}} ->
      {ok, #state{ name = Name
                 , port_read_char = PortReadChar
                 , port_close = PortClose
                 , port_state = PortState
                 , peeked = []
                 , line = 1
                 , column = 0
                 }};
    {error, Reason} ->
      %% The {shutdown, ...} wrapper prevents an unwanted crash report.
      {stop, {shutdown, Reason}}
  end.

handle_call(Req, _From, State) ->
  case Req of
    ?close ->
      Result = handle_close(State),
      {stop, normal, Result, []};
    ?column ->
      Result = handle_column(State),
      {reply, Result, State};
    ?line ->
      Result = handle_line(State),
      {reply, Result, State};
    ?name ->
      Result = handle_name(State),
      {reply, Result, State};
    ?peek_char ->
      {Result, NewState} = handle_peek_char(State),
      {reply, Result, NewState};
    ?read_char ->
      {Result, NewState} = handle_read_char(State),
      {reply, Result, NewState};
    _ ->
      {reply, {error, {bad_call, Req}}, State}
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

handle_init(Arg) ->
  case Arg of
    {?file, Path} -> open_file_port(Path);
    ?stdin -> open_stdin_port();
    {?string, String} -> open_string_port(String)
  end.

handle_close(State) ->
  {ok, port_close(State)}.

handle_column(State) ->
  {ok, State#state.column}.

handle_line(State) ->
  {ok, State#state.line}.

handle_name(State) ->
  {ok, State#state.name}.

handle_peek_char(State) ->
  case State#state.peeked of
    [] ->
      {Ch, NewState} = port_read_char(State),
      {{ok, Ch}, NewState#state{peeked = Ch}};
    Ch ->
      {{ok, Ch}, State}
  end.

handle_read_char(State0) ->
  {Ch, State} =
    case State0#state.peeked of
      [] -> port_read_char(State0);
      Peeked -> {Peeked, State0#state{peeked = []}}
    end,
  NewState =
    case Ch of
      $\n ->
        Line = State#state.line,
        State#state{line = Line + 1, column = 0};
      $\t ->
        Column = State#state.column,
        State#state{column = ((Column + 8) div 8) * 8};
      -1 ->
        State;
      _ ->
        Column = State#state.column,
        State#state{column = Column + 1}
    end,
  {{ok, Ch}, NewState}.

%% port operations -------------------------------------------------------------

open_file_port(Path) ->
  case file:open(Path, [read, {encoding, utf8}, read_ahead]) of
    {ok, IoDev} ->
      {ok, {filename:basename(Path), fun iodev_read_char/1, fun iodev_close/1, IoDev}};
    {error, Reason} ->
      {error, {file, Reason}}
  end.

open_stdin_port() ->
  {ok, {"<stdin>", fun iodev_read_char/1, fun noop_close/1, standard_io}}.

open_string_port(String) ->
  {ok, {"", fun string_read_char/1, fun noop_close/1, String}}.

port_read_char(State) ->
  (State#state.port_read_char)(State).

-compile({no_auto_import, [port_close/1]}).
port_close(State) ->
  (State#state.port_close)(State).

iodev_read_char(State = #state{port_state = IoDev}) ->
  Ch =
    case io:get_chars(IoDev, [], 1) of
      [Ch0] -> Ch0;
      eof  -> -1
    end,
  {Ch, State}.

iodev_close(#state{port_state = IoDev}) ->
  case file:close(IoDev) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

string_read_char(State = #state{port_state = String}) ->
  case String of
    [Ch | Rest] ->
      {Ch, State#state{port_state = Rest}};
    [] ->
      {-1, State}
  end.

noop_close(_State) ->
  ok.
