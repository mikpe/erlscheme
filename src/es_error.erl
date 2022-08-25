%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2019-2022 Mikael Pettersson
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
%%% es_error.erl
%%%
%%% Format error terms for output in end-user visible diagnostics.
%%%
%%% Our standard representation of error terms is as {Module, Reason} 2-tuples,
%%% where Module:format_error(Reason) returns a textual representation of Reason.
%%% These error terms are typically returned as {error, {Module, Reason}} values
%%% or thrown as error exceptions.
%%%
%%% This code will _not_ attempt to load Module.

-module(es_error).

-export([format/1]).

-spec format(term()) -> io_lib:chars().
format({Module, Reason} = Error) when is_atom(Module) ->
  case erlang:function_exported(Module, format_error, 1) of
    true ->
      try Module:format_error(Reason)
      catch _:_ -> default_format(Error)
      end;
    false -> default_format(Error)
  end;
format(Error) -> default_format(Error).

default_format(Error) ->
  io_lib:format("~tp", [Error]).
