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
%%% es_print.erl
%%%
%%% A term printer for ErlScheme.

-module(es_print).

-export([ display/1
        , write/1
        ]).

-type datum() :: term().

%% API -------------------------------------------------------------------------

-spec display(datum()) -> ok.
display(Term) ->
  print(Term, _Depth = 10, _Width = 20, _IsDisplay = true).

-spec write(datum()) -> ok.
write(Term) ->
  Infinity = infinity(),
  print(Term, _Depth = Infinity, _Width = Infinity, _IsDisplay = false).

%% Internals -------------------------------------------------------------------

%% For Depth and Width limited output we need Limit values that act
%% like finite non-negative integers with "decrement" and "is-zero"
%% operations, while also supporting a reserved value representing
%% positive infinity for unlimited output.  We use "-1" as +infinity.

-define(infinity, -1).

infinity() -> ?infinity.

decrement(?infinity) -> ?infinity;
decrement(N) when N > 0 -> N - 1.

print(_, 0, _, _) ->
  io:format("...");
print(Term, DepthLim, WidthLim, IsDisplay) ->
  case Term of
    [Hd | Tl] ->
      io:format("("),
      print_list(Hd, Tl, WidthLim, DepthLim, WidthLim, IsDisplay),
      io:format(")");
    [] -> io:format("()");
    true -> io:format("#t");
    false -> io:format("#f");
    _ when is_number(Term) -> io:format("~tp", [Term]); % includes characters
    _ when is_atom(Term) -> print_symbol(Term, IsDisplay);
    _ when is_function(Term) -> % includes eof-object and ports
      io:format("#<subr ~ts>", [erlang:fun_to_list(Term)]);
    _ when is_binary(Term) -> print_string(Term, IsDisplay);
    _ when is_tuple(Term) -> print_tuple(Term, DepthLim, WidthLim, IsDisplay)
  end.

print_list(_, _, 0, _, _, _) ->
  io:format("...)");
print_list(Hd, Tl, WL, DepthLim, WidthLim, IsDisplay) ->
  print(Hd, decrement(DepthLim), WidthLim, IsDisplay),
  case Tl of
    [] ->
      [];
    [Hd2 | Tl2] ->
      io:format(" "),
      print_list(Hd2, Tl2, decrement(WL), DepthLim, WidthLim, IsDisplay);
    _ ->
      io:format(" . "),
      case decrement(WL) of
        0 -> io:format("...");
        _ -> print(Tl, DepthLim, WidthLim, IsDisplay)
      end
  end.

print_tuple(Tuple, DepthLim, WidthLim, IsDisplay) ->
  io:format("#("),
  print_vector(Tuple, 0, DepthLim, WidthLim, IsDisplay),
  io:format(")").

print_vector(Tuple, I, DepthLim, WidthLim, IsDisplay) ->
  if I + 1 > size(Tuple) ->
      [];
     true ->
      if I > 0 -> io:format(" ");
         true -> []
      end,
      if I >= WidthLim ->
          io:format("...");
         true ->
          print(element(I + 1, Tuple), decrement(DepthLim), WidthLim, IsDisplay),
          print_vector(Tuple, I + 1, DepthLim, WidthLim, IsDisplay)
      end
  end.

print_string(Binary, IsDisplay) ->
  case IsDisplay of
    true ->
      io:format("~ts", [Binary]);
    false ->
      io:format("\""),
      escape_string(unicode:characters_to_list(Binary)),
      io:format("\"")
  end.

escape_string([]) -> [];
escape_string([Ch | Rest]) ->
  case Ch of
    7 ->
      io:format("\\a");
    8 ->
      io:format("\\b");
    9 ->
      io:format("\\t");
    10 ->
      io:format("\\n");
    13 ->
      io:format("\\r");
    34 ->
      io:format("\\\"");
    92 ->
      io:format("\\\\");
    _ when Ch < 32; Ch >= 127 ->
      io:format("\\x~.16B;", [Ch]);
    _ ->
      io:format("~tc", [Ch])
  end,
  escape_string(Rest).

print_symbol(Symbol, IsDisplay) ->
  case IsDisplay of
    true ->
      io:format("~tp", [Symbol]);
    false ->
      Pname = atom_to_list(Symbol),
      case pname_needs_escape(Pname) of
        false ->
          io:format("~tp", [Symbol]);
        true ->
          escape_pname(Pname)
      end
  end.

pname_needs_escape([]) -> true;
pname_needs_escape([Ch | Rest]) ->
  case es_ctype:char_is_initial(Ch) of
    true -> pname_rest_needs_escape(Rest);
    false -> true
  end.

pname_rest_needs_escape([]) -> false;
pname_rest_needs_escape([Ch | Rest]) ->
  case es_ctype:char_is_subsequent(Ch) of
    true -> pname_rest_needs_escape(Rest);
    false -> true
  end.

escape_pname([]) -> io:format("||");
escape_pname([Ch | Rest]) ->
  case es_ctype:char_is_initial(Ch) of
    true -> io:format("|~tc", [Ch]);
    false -> io:format("|\\x~.16B;", [Ch])
  end,
  escape_pname_rest(Rest).

escape_pname_rest([]) -> io:format("|");
escape_pname_rest([Ch | Rest]) ->
  case es_ctype:char_is_subsequent(Ch) of
    true -> io:format("~tc", [Ch]);
    false -> io:format("\\x~.16B;", [Ch])
  end,
  escape_pname_rest(Rest).
