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
%%% es_env.erl
%%%
%%% Var -> Val environment abstraction for ErlScheme.

-module(es_env).

-export([empty/0, enter/3, get/2, lookup/2, map/2, overlay/2]).

empty() -> gb_trees:empty().
get(Env, Var) -> gb_trees:get(Var, Env).
lookup(Env, Var) -> gb_trees:lookup(Var, Env).
enter(Env, Var, Val) -> gb_trees:enter(Var, Val, Env).

map(Env, Fn) -> gb_trees:map(Fn, Env).

overlay(Env1, Env2) ->
  overlay_iter(Env1, gb_trees:iterator(Env2)).
overlay_iter(Env, Iter1) ->
  case gb_trees:next(Iter1) of
    none ->
      Env;
    {Var, Val, Iter2} ->
      overlay_iter(gb_trees:enter(Var, Val, Env), Iter2)
  end.
