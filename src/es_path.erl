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
%%% es_path.erl
%%%
%%% Module to help ErlScheme locate its private files

-module(es_path).

-export([lib_dir/1]).

lib_dir(SubDir) ->
  PrivDir = code:priv_dir(erlscheme),
  LibDir = filename:join(PrivDir, SubDir),
  case filelib:is_dir(LibDir) of
    true -> {ok, LibDir};
    false -> {error, notfound}
  end.
