%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2017-2022 Mikael Pettersson
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
%%% es_compile.erl
%%%
%%% Compile ErlScheme modules to BEAM files.

-module(es_compile).

-export([ file/1
        , file/2
        ]).

-type datum() :: term().

%% API -------------------------------------------------------------------------

-spec file(datum()) -> ok.
file(Arg) ->
  file(Arg, _Opts = []).

-spec file(datum(), proplists:proplist()) -> ok.
file(Arg, Opts) ->
  %% Since we want to replace the extension of the file name with ++,
  %% we need it to be an Erlang string(), i.e. list().
  FileName = unicode:characters_to_list(Arg),
  AST = es_load:module(FileName),
  BaseName = filename:basename(FileName, ".scm"),
  case proplists:get_bool(save_ast, Opts) of
    true ->
      ok = file:write_file(BaseName ++ ".ast", io_lib:format("~tp\n", [AST]));
    false ->
      ok
  end,
  CerlModule = es_ast_to_core:module(AST),
  {ok, _} = core_lint:module(CerlModule),
  case proplists:get_bool(save_core, Opts) of
    true ->
      ok = file:write_file(BaseName ++ ".core",
                           io_lib:format("~ts\n", [core_pp:format(CerlModule)]));
    false ->
      ok
  end,
  {ok, _ModuleName, BeamBin} = compile:forms(CerlModule, [from_core, verbose, report_errors, report_warnings]),
  BeamName = BaseName ++ ".beam",
  BeamPath =
    case lists:keyfind(outdir, 1, Opts) of
      {outdir, OutDir} -> filename:join(OutDir, BeamName);
      false -> BeamName
    end,
  ok = file:write_file(BeamPath, BeamBin),
  ok.
