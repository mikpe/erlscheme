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
%%% es_datum.erl
%%%
%%% Maps between Scheme datums and their Erlang representations.
%%%
%%% Scheme              Erlang
%%% ======              ======
%%%
%%% Straight-forward mappings:
%%%
%%% null                []
%%% number              integer() or float()
%%% symbol              atom() except boolean()
%%%
%%% Slightly non-obvious mappings:
%%%
%%% pair                [_ | _]
%%%
%%% Scheme pairs are similar to Erlang list cells, except
%%% for the fact that list cells are immutable.  We map pairs
%%% to list cells, and accept that mutation is unavailable.
%%%
%%% #t                  true
%%% #f                  false
%%%
%%% Scheme requires booleans to be disjoint from symbols,
%%% but Erlang considers booleans to be special-case atoms.
%%% We follow the Erlang convention.
%%%
%%% vector              tuple()
%%%
%%% Scheme vectors are similar to Erlang tuples, except
%%% for the fact that tuples are immutable.  We map vectors
%%% to tuples, and accept that mutation is unavailable.
%%%
%%% character           char()
%%%
%%% Scheme requires characters to be a distinct type, but
%%% Erlang considers them to be a sub-range of the integers.
%%% We represent them as unadorned integers.
%%%
%%% eof-object          fun es_datum:the_eof_object/0
%%%
%%% Scheme requires the eof-object to be a distinct type.
%%% We relax that requirement and represent it as procedure
%%% referencing es_datum:the_eof_object/0.
%%% R5RS did not require this to be a distinct type.
%%%
%%% unspecified         []
%%%
%%% Scheme specifies that certain expressions evaluate to an
%%% unspecified value.  For simplicity we use '() for that.
%%%
%%% string              binary()
%%% bytevector          <NYI -- should be binary()>
%%%
%%% Scheme requires strings and bytevectors to be distinct types,
%%% but Erlang considers strings to be lists of characters while
%%% bytevectors closely resemble Erlang binaries.  We represent both
%%% as unadorned binaries, and accept that mutation is unavailable.
%%% R5RS did not have bytevectors, they were added in R6RS and R7RS.
%%%
%%% port                <an arity-0 closure from module es_port_wrapper>
%%%
%%% [NYI]
%%% Scheme requires ports to be a distinct type.  We relax that
%%% requirement and represent a port as an arity-0 function closure
%%% that returns the corresponding handle (Pid).  The function closures
%%% need to come from a reserved module to enable checking their type.
%%%
%%% procedure           Fun/N
%%%
%%% An Scheme procedure becomes an Erlang function of the same arity.
%%% Variable-arity procedures are not supported.
%%%
%%% tid                 pid
%%%
%%% RnRS Scheme does not have threads, but ErlScheme adds threads
%%% and maps them to Erlang processes.
%%%
%%% TODO:
%%% - R6RS record types?
%%% - R6RS exception values?

-module(es_datum).

%% API
-export([ binary_to_string/1
        , is_eof_object/1
        , is_string/1
        , is_symbol/1
        , is_vector/1
        , integer_to_char/1
        , list_to_vector/1
        , mk_eof_object/0
        , string_to_binary/1
        , unspecified/0
        ]).

%% private exports
-export([ the_eof_object/0
        ]).

%% API -------------------------------------------------------------------------

%% Characters

integer_to_char(I) -> I.

%% EOF object

-define(the_eof_object, the_eof_object).

is_eof_object(X) ->
  case is_function(X, 0) of
    true ->
      case erlang:fun_info(X, name) of
        {name, ?the_eof_object} -> {module, ?MODULE} =:= erlang:fun_info(X, module);
        {name, _} -> false
      end;
    false ->
      false
  end.

%% This has to return an exported fun to make fun_info(_, name) well-defined.
mk_eof_object() -> fun ?MODULE:?the_eof_object/0.

?the_eof_object() -> error("eof-object was called").

%% Strings

is_string(X) -> is_binary(X).

binary_to_string(B) -> B.

string_to_binary(S) -> S.

%% Symbols

is_symbol(X) -> is_atom(X) andalso not erlang:is_boolean(X).

%% Vectors

is_vector(X) -> is_tuple(X).

list_to_vector(L) -> erlang:list_to_tuple(L).

%% Unspecified

unspecified() -> [].
