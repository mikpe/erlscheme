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
%%% es_env.erl
%%%
%%% Var -> Val environment abstraction for ErlScheme.

-module(es_env).

-export([ empty/0
        , enter/3
        , get/2
        , is_bound/2
        , lookup/2
        , map/2
        , overlay/2
        ]).

-export_type([ env/0
             ]).

-type env() :: map().

-spec empty() -> env().
empty() ->
  maps:new().

-spec get(env(), any()) -> any().
get(Env, Var) ->
  maps:get(Var, Env).

-spec enter(env(), any(), any()) -> env().
enter(Env, Var, Val) ->
  maps:put(Var, Val, Env).

-spec is_bound(env(), any()) -> boolean().
is_bound(Env, Var) ->
  maps:is_key(Var, Env).

-spec lookup(env(), any()) -> none | {value, any()}.
lookup(Env, Var) ->
  case maps:is_key(Var, Env) of
    true -> {value, maps:get(Var, Env)};
    false -> none
  end.

-spec map(env(), fun((any(), any()) -> any())) -> env().
map(Env, Fn) ->
  maps:map(Fn, Env).

-spec overlay(env(), env()) -> env().
overlay(Env1, Env2) ->
  maps:merge(Env1, Env2).
