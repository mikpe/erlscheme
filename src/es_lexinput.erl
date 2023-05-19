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

-spec open_string(string() | binary()) -> lexinput().
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
        { %% name and access functions don't change after init
          name
        , close
          %% standard Erlang I/O device handle
        , iodev
          %% if peeked is =/= [] it is the value of the last retrieved character,
          %% which was peeked not read, and line and column have not been updated
        , peeked
          %% line and column give the position of the next character to be retrieved
        , line
        , column
        }).

init(Arg) ->
  case handle_open(Arg) of
    {ok, {Name, Close, IoDev}} ->
      {ok, #state{ name = Name
                 , close = Close
                 , iodev = IoDev
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

handle_open(Arg) ->
  case Arg of
    {?file, Path} -> file_open(Path);
    ?stdin -> stdin_open();
    {?string, String} -> string_open(String)
  end.

handle_close(State) ->
  {ok, iodev_close(State)}.

handle_column(State) ->
  {ok, State#state.column}.

handle_line(State) ->
  {ok, State#state.line}.

handle_name(State) ->
  {ok, State#state.name}.

handle_peek_char(State) ->
  case State#state.peeked of
    [] ->
      Ch = iodev_read_char(State),
      {{ok, Ch}, State#state{peeked = Ch}};
    Ch ->
      {{ok, Ch}, State}
  end.

handle_read_char(State0) ->
  {Ch, State} =
    case State0#state.peeked of
      [] -> {iodev_read_char(State0), State0};
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

%% IoDev operations ------------------------------------------------------------

iodev_close(#state{close = Close, iodev = IoDev}) ->
  Close(IoDev).

iodev_read_char(#state{iodev = IoDev}) ->
  case io:get_chars(IoDev, [], 1) of
    [Ch] -> Ch;
    eof  -> -1
  end.

%% file operations -------------------------------------------------------------

file_open(Path) ->
  case file:open(Path, [read, {encoding, utf8}, read_ahead]) of
    {ok, IoDev} ->
      {ok, {filename:basename(Path), fun file_close/1, IoDev}};
    {error, Reason} ->
      {error, {file, Reason}}
  end.

file_close(IoDev) ->
  case file:close(IoDev) of
    ok -> ok;
    {error, Reason} -> {error, {file, Reason}}
  end.

%% stdin operations ------------------------------------------------------------

stdin_open() ->
  {ok, {"<stdin>", fun noop_close/1, standard_io}}.

noop_close(_IoDev) ->
  ok.

%% string operations -----------------------------------------------------------

string_open(String) ->
  IoDev = es_input_string_iodev:open(String),
  {ok, {"<string>", fun es_input_string_iodev:close/1, IoDev}}.
