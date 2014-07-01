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
%%% es_path.erl
%%%
%%% Module to help ErlScheme locate its private files

-module(es_path).

-export([lib_dir/1]).

%% Alas, code:lib_dir(?MODULE, SubDir) doesn't work unless
%% you're a full-blown OTP application, so we roll our own
%% by searching code:get_path().

lib_dir(SubDir) ->
  ModName = atom_to_list(?MODULE) ++ ".beam",
  lib_dir(code:get_path(), ModName, SubDir).

lib_dir([], _, _) -> {error, notfound};
lib_dir([Dir | Dirs], ModName, SubDir) ->
  case file:read_file_info(filename:join([Dir, ModName])) of
    {error, _} ->
      lib_dir(Dirs, ModName, SubDir);
    {ok, _} ->
      Dir2 = filename:join([Dir, "..", SubDir]),
      case filelib:is_dir(Dir2) of
	false ->
	  lib_dir(Dirs, ModName, SubDir);
	true ->
	  {ok, Dir2}
      end
  end.
