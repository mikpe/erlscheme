%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2014-2017 Mikael Pettersson
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
%%% es_datum.erl
%%%
%%% Maps between Scheme datums and their Erlang representations.
%%%
%%% Scheme		Erlang
%%% ======		======
%%%
%%% Straight-forward mappings:
%%%
%%% null		[]
%%% number		integer or float
%%% symbol		atom (with some exceptions)
%%%
%%% Slightly non-obvious mappings:
%%%
%%% pair		[_ | _]
%%%
%%% Scheme pairs are similar to Erlang list cells, except
%%% for the fact that list cells are immutable.  We map pairs
%%% to list cells, and accept that mutation is unavailable.
%%%
%%% #t			true
%%% #f			false
%%%
%%% Scheme requires booleans to be disjoint from symbols,
%%% but Erlang considers booleans to be special-case atoms.
%%% We follow the Erlang convention.
%%%
%%% vector		{'ES:VECTOR', Elements...}
%%%
%%% Scheme vectors are similar to Erlang tuples, except
%%% for the fact that tuples are immutable.
%%% We could map vectors to tuples as-is, but several other types
%%% require us to use tagged tuples (tuples with an atom in the
%%% first element acting as a type tag), so to distinguish vectors
%%% from those we also represent vectors as tagged tuples, and
%%% accept that mutation is unavailable.
%%%
%%% character		{'ES:CHAR', Integer}
%%%
%%% Scheme requires characters to be a distinct type, but
%%% Erlang considers them to be a sub-range of the integers.
%%% We represent them as integers inside tagged tuples.
%%%
%%% eof-object		{}
%%%
%%% Scheme requires the eof-object to be a distinct type.
%%% All other tuples we generate are tagged and thus non-empty,
%%% so we can use the empty tuple for this special object.
%%% R5RS did not require this to be a distinct type.
%%% [XXX: represent EOF as {'ES:CHAR', -1} instead?]
%%%
%%% string		{'ES:STRING', Binary}
%%% bytevector		{'ES:BYTEVECTOR', Binary}
%%%
%%% Scheme requires strings and bytevectors to be distinct types,
%%% but Erlang considers strings to be lists of characters while
%%% bytevectors closely resemble Erlang binaries.  We represent
%%% both as binaries inside tagged tuples, and accept that mutation
%%% is unavailable.
%%% R5RS did not have bytevectors, they were added in R6RS and R7RS.
%%%
%%% port		{'ES:PORT', PortHandle}
%%%
%%% Scheme requires ports to be a distinct type, but Erlang has no
%%% corresponding type.  We represent them as our private handles
%%% inside tagged tuples.
%%%
%%% procedure		Fun/1
%%%
%%% An ErlScheme procedure becomes an arity-1 Erlang function.
%%%
%%% tid			pid
%%%
%%% RnRS Scheme does not have threads, but ErlScheme adds threads
%%% and maps them to Erlang processes.
%%%
%%% TODO:
%%% - R6RS record types?
%%% - R6RS exception values?

-module(es_datum).

%% Booleans
-export([is_boolean/1]).

%% Symbols
-export([is_symbol/1]).

%% The EOF object
-export([is_eof_object/1,
	 mk_eof_object/0]).

%% Vectors
-export([is_vector/1,
	 list_to_vector/1,
	 vector_ref/2,
	 vector_set/3]).

%% Characters
-export([is_char/1,
	 integer_to_char/1,
	 char_to_integer/1]).

%% Strings
-export([is_string/1,
	 binary_to_string/1,
	 string_to_binary/1]).

%% Bytevectors
-export([is_bytevector/1,
	 binary_to_bytevector/1,
	 bytevector_to_binary/1]).

%% Ports
-export([is_port/1,
	 handle_to_port/1,
	 port_to_handle/1]).

%% Booleans

is_boolean(X) ->
  if X =:= true; X =:= false -> true;
     true -> false
  end.

%% Symbols

is_symbol(X) ->
  if is_atom(X), X =/= 'false', X =/= 'true' -> true;
     true -> false
  end.

%% The EOF object

is_eof_object(X) -> case X of {} -> true; _ -> false end.
mk_eof_object() -> {}.

%% Vectors

is_vector(X) ->
  if is_tuple(X), size(X) >= 1, element(1, X) =:= 'ES:VECTOR' -> true;
     true -> false
  end.

list_to_vector(L) ->
  erlang:list_to_tuple(['ES:VECTOR' | L]).

vector_ref(V, I) -> element(I + 2, V).

vector_set(V, I, X) -> setelement(I + 2, V, X).

%% Characters

is_char(X) ->
  if is_tuple(X), size(X) =:= 2, element(1, X) =:= 'ES:CHAR' -> true;
     true -> false
  end.

integer_to_char(I) -> {'ES:CHAR', I}.

char_to_integer({'ES:CHAR', I}) -> I.

%% Strings

is_string(X) ->
  if is_tuple(X), size(X) =:= 2, element(1, X) =:= 'ES:STRING' -> true;
     true -> false
  end.

binary_to_string(B) -> {'ES:STRING', B}.

string_to_binary(S) -> element(2, S).

%% Bytevectors

is_bytevector(X) ->
  if is_tuple(X), size(X) =:= 2, element(1, X) =:= 'ES:BYTEVECTOR' -> true;
     true -> false
  end.

binary_to_bytevector(B) -> {'ES:STRING', B}.

bytevector_to_binary(S) -> element(2, S).

%% Ports

is_port(X) ->
  if is_tuple(X), size(X) =:= 2, element(1, X) =:= 'ES:PORT' -> true;
     true -> false
  end.

handle_to_port(H) -> {'ES:PORT', H}.

port_to_handle(P) -> element(2, P).
