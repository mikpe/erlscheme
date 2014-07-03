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
%%% es_lexinput.erl
%%%
%%% Wraps a raw input port with state to maintain line number and column,
%%% and to remember the port's file name (if any).  Like raw input ports,
%%% the state is maintained in a separate Erlang process.

-module(es_lexinput).

-export([open/2,
	 close/1,
	 read_char/1,
	 peek_char/1,
	 line/1,
	 column/1,
	 name/1]).

-record(state,
	{port,
	 line,
	 column,
	 name}).

handle_loop(State) ->
  receive
    {Pid, Cmd} ->
      case Cmd of
	'read_char' ->
	  handle_read_char(State, Pid);
	'peek_char' ->
	  handle_peek_char(State, Pid);
	'line' ->
	  handle_line(State, Pid);
	'column' ->
	  handle_column(State, Pid);
	'name' ->
	  handle_name(State, Pid);
	'close' ->
	  handle_close(State, Pid);
	_ ->
	  handle_yield_continue(State, Pid, {error, Cmd})
      end;
    _ ->
      handle_loop(State)
  end.

handle_yield_continue(State, Pid, R) ->
  Pid ! {self(), R},
  handle_loop(State).

%% XXX: for \r bump Line and reset Column like for \n, but also set a flag,
%% then in \n check that flag and if set do not bump Line or reset Column,
%% for any Ch not \r reset the \r flag

handle_read_char(State0, Pid) ->
  Ch = es_raw_port:read_char(State0#state.port),
  State1 =
    case Ch of
      $\n ->
	Line = State0#state.line,
	State0#state{line = Line + 1, column = 0};
      $\t ->
	Column = State0#state.column,
	State0#state{column = ((Column + 8) div 8) * 8};
      -1 ->
	State0;
      _ ->
	Column = State0#state.column,
	State0#state{column = Column + 1}
    end,
  handle_yield_continue(State1, Pid, {ok, Ch}).

handle_peek_char(State, Pid) ->
  Ch = es_raw_port:peek_char(State#state.port),
  handle_yield_continue(State, Pid, {ok, Ch}).

handle_line(State, Pid) ->
  Line = State#state.line,
  handle_yield_continue(State, Pid, {ok, Line}).

handle_column(State, Pid) ->
  Column = State#state.column,
  handle_yield_continue(State, Pid, {ok, Column}).

handle_name(State, Pid) ->
  Name = State#state.name,
  handle_yield_continue(State, Pid, {ok, Name}).

handle_close(State, Pid) ->
  es_raw_port:close_input_port(State#state.port),
  Pid ! {self(), {ok, true}}. % no continue -> terminate process

command(Pid, Cmd) ->
  MonRef = erlang:monitor('process', Pid),
  Pid ! {self(), Cmd},
  Res =
    receive
      {Pid, X} ->
	erlang:demonitor(MonRef, ['flush']),
	X;
      {'DOWN', MonRef, 'process', Pid, _} ->
	{error, noproc}
    end,
  {ok, Val} = Res, % deliberately throws in case of error
  Val.

open(Port, Name) ->
  State = #state{port = Port, line = 1, column = 0, name = Name},
  spawn(fun () -> handle_loop(State) end).

read_char(Pid) ->
  command(Pid, 'read_char').

peek_char(Pid) ->
  command(Pid, 'peek_char').

line(Pid) ->
  command(Pid, 'line').

column(Pid) ->
  command(Pid, 'column').

name(Pid) ->
  command(Pid, 'name').

close(Pid) ->
  command(Pid, 'close').
