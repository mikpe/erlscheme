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
%%% es_print.erl
%%%
%%% A term printer for ErlScheme.

-module(es_print).

-export([write/1, display/1]).

write(Term) ->
  print(Term, 0, infinity, false).

display(Term) ->
  print(Term, 0, 10, true).

-ifdef(notdef).
print(Term, CurDepth, DepthLim, IsDisplay) ->
  case Term of
    [Hd | Tl] -> print_list(Hd, Tl, CurDepth, DepthLim, IsDisplay);
    _ when is_tuple(Term) -> print_tuple(Term, CurDepth, DepthLim, IsDisplay);
    _ when is_atom(Term) -> print_atom(Term);
    [] -> io:format("()");
    _ -> io:format("~p", [Term])
  end.

print_tuple(Tuple, CurDepth, DepthLim, IsDisplay) ->
  case Tuple of
    {} -> io:format("#\eof-object");
    {'ES:CHAR', Ch} -> ;
    {'ES:STRING', Binary} -> ;
    {'ES:BYTEVECTOR', Binary} -> ;
    {'ES:PORT', PortHandle} -> ;
    {'ES:ERLFUNC', Fun} -> ;
    {'ES:CLOSURE', _Formals, _Body, _Env, _RecEnv} ->
  end.

print_atom(Atom) ->
  case Atom of
    true -> io:format("#t");
    false -> io:format("#f");
    _ -> io:format("~p", [Atom])
  end.
-else.
print(Term, _, _, _) -> io:format("~p", [Term]).
-endif.
