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

-export([read_char/1,
	 peek_char/1,
	 close_input_port/1,
	 open_input_file/1,
	 open_input_string/1,
	 open_stdin/0]).

-export([write_char/2,
	 close_output_port/1,
	 open_stdout/0]).

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

ktrue(_) -> {ok, true}.

%% Generic input port infrastructure

-record(input_port_fns,
	{close,
	 read_char,
	 peek_char}).

handle_input_port(State0, Fns) ->
  receive
    {Pid, Cmd} when is_pid(Pid), is_atom(Cmd) ->
      case Cmd of
	'read_char' ->
	  {R, State1} = (Fns#input_port_fns.read_char)(State0),
	  Pid ! {self(), R},
	  handle_input_port(State1, Fns);
	'peek_char' ->
	  {R, State1} = (Fns#input_port_fns.peek_char)(State0),
	  Pid ! {self(), R},
	  handle_input_port(State1, Fns);
	'close' ->
	  R = (Fns#input_port_fns.close)(State0),
	  Pid ! {self(), R};
	_ ->
	  R = {error, Cmd},
	  Pid ! {self(), R},
	  handle_input_port(State0, Fns)
      end;
    _ ->
      handle_input_port(State0, Fns)
  end.

open_input_port(State, Fns) ->
  spawn(fun () -> handle_input_port(State, Fns) end).

read_char(Pid) ->
  command(Pid, 'read_char').

peek_char(Pid) ->
  command(Pid, 'peek_char').

close_input_port(Pid) ->
  command(Pid, 'close').

%% String input ports

open_input_string(S) ->
  State = {list_to_binary(S), 0},
  Fns = #input_port_fns
    {close = fun ktrue/1,
     read_char = fun input_string_read_char/1,
     peek_char = fun input_string_peek_char/1},
  open_input_port(State, Fns).

input_string_read_char(S = {B, I}) ->
  if I < size(B) ->
      V = binary:at(B, I),
      S2 = {B, I + 1},
      {{ok, V}, S2};
     true ->
      V = -1,
      {{ok, V}, S}
  end.

input_string_peek_char(S = {B, I}) ->
  V = if I < size(B) ->
	  binary:at(B, I);
	 true ->
	  -1
      end,
  {{ok, V}, S}.

%% File input ports

open_input_file(Path) ->
  %% We MUST call file:open/2 from within the wrapper process,
  %% so this open-codes open_input_port/2.  A positive consequence
  %% is that we then may open the file in 'raw' mode.
  P = spawn(fun () -> do_open_input_file(Path) end),
  %% Verify that the open succeeded.
  _ = peek_char(P),
  P.

do_open_input_file(Path) ->
  {ok, IoDev} = file:open(Path, [read, raw, read_ahead]),
  State = {[], IoDev},
  Fns = #input_port_fns
    {close = fun input_file_close/1,
     read_char = fun input_file_read_char/1,
     peek_char = fun input_file_peek_char/1},
  handle_input_port(State, Fns).

input_file_close({_, IoDev}) ->
  case file:close(IoDev) of
    ok -> {ok, true};
    Error -> Error
  end.

input_file_read_char({Buf, IoDev}) ->
  case Buf of
    [] ->
      case file:read(IoDev, 1) of
	{ok, [Ch | Rest]} -> {{ok, Ch}, {Rest, IoDev}};
	eof -> {{ok, -1}, {[], IoDev}}
      end;
    [Ch | Rest] ->
      {{ok, Ch}, {Rest, IoDev}}
  end.

input_file_peek_char(State = {Buf, IoDev}) ->
  case Buf of
    [] ->
      case file:read(IoDev, 1) of
	{ok, Line = [Ch | _]} -> {{ok, Ch}, {Line, IoDev}};
	eof -> {{ok, -1}, {[], IoDev}}
      end;
    [Ch | _] ->
      {{ok, Ch}, State}
  end.

%% Standard input port

open_stdin() ->
  State = [],
  Fns = #input_port_fns
    {close = fun ktrue/1,
     read_char = fun stdin_read_char/1,
     peek_char = fun stdin_peek_char/1},
  open_input_port(State, Fns).

stdin_read_char(State) ->
  case State of
    [] ->
      case io:get_line(standard_io, []) of
	[Ch | Rest] -> {{ok, Ch}, Rest};
	eof -> {{ok, -1}, []}
      end;
    [Ch | Rest] ->
      {{ok, Ch}, Rest}
  end.

stdin_peek_char(State) ->
  case State of
    [] ->
      Line = io:get_line(standard_io, []),
      case Line of
	[Ch | _] -> {{ok, Ch}, Line};
	eof -> {{ok, -1}, []}
      end;
    [Ch | _] ->
      {{ok, Ch}, State}
  end.

%% Generic output port infrastructure

-record(output_port_fns,
	{close,
	 write_char}).

handle_output_port(State0, Fns) ->
  receive
    {Pid, Cmd} when is_pid(Pid) ->
      case Cmd of
	{'write_char', Ch} ->
	  {R, State1} = (Fns#output_port_fns.write_char)(State0, Ch),
	  Pid ! {self(), R},
	  handle_output_port(State1, Fns);
	'close' ->
	  R = (Fns#output_port_fns.close)(State0),
	  Pid ! {self(), R};
	_ ->
	  R = {error, Cmd},
	  Pid ! {self(), R},
	  handle_output_port(State0, Fns)
      end;
    _ ->
      handle_output_port(State0, Fns)
  end.

open_output_port(State, Fns) ->
  spawn(fun () -> handle_output_port(State, Fns) end).

write_char(Pid, Ch) ->
  command(Pid, {'write_char', Ch}).

close_output_port(Pid) ->
  command(Pid, 'close').

%% Standard output port

open_stdout() ->
  State = [],
  Fns = #output_port_fns
    {close = fun ktrue/1,
     write_char = fun stdout_write_char/2},
  open_output_port(State, Fns).

stdout_write_char(State, Ch) ->
  io:put_chars(standard_io, [Ch]),
  {{ok, true}, State}.
