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
%%% es_lexinput.erl
%%%
%%% Wraps a raw input port with state to maintain line number and column,
%%% and to remember the port's file name (if any).  Like raw input ports,
%%% the state is maintained in a separate Erlang process.

-module(es_lexinput).
-behaviour(gen_server).

%% API
-export([ close/1
        , column/1
        , line/1
        , name/1
        , open/2
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
-define(line, line).
-define(name, name).
-define(peek_char, peek_char).
-define(read_char, read_char).

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

-spec open(es_raw_port:es_port(), file:filename_all()) -> lexinput().
open(Port, Name) ->
  %% deliberately throw in case of error
  {ok, Pid} = gen_server:start(?MODULE, {Port, Name}, []),
  Pid.

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

%% gen_server callbacks --------------------------------------------------------

-record(state,
        { port
        , line
        , column
        , name
        }).

init({Port, Name}) ->
  {ok, #state{port = Port, line = 1, column = 0, name = Name}}.

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
      Result = handle_peek_char(State),
      {reply, Result, State};
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

handle_close(State) ->
  {ok, es_raw_port:close_input_port(State#state.port)}.

handle_column(State) ->
  {ok, State#state.column}.

handle_line(State) ->
  {ok, State#state.line}.

handle_name(State) ->
  {ok, State#state.name}.

handle_peek_char(State) ->
  {ok, es_raw_port:peek_char(State#state.port)}.

handle_read_char(State) ->
  Ch = es_raw_port:read_char(State#state.port),
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
