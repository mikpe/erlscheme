%%% -*- erlang-indent-level: 2 -*-
%%%
%%%   Copyright 2022 Mikael Pettersson
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
%%% es_synenv.erl
%%%
%%% Syntax Environment for ErlScheme.
%%%
%%% - supports a subset of standard environment (es_env API) operations
%%%   (empty/0, lookup/2, enter/3)
%%% - supports layering a side-effect free env (es_env) on top of a stateful env
%%%   (es_gloenv), this is used for nested scopes in the repl / user environment

-module(es_synenv).

%% standard es_env API (subset)
-export([ empty/0
        , enter/3
        , lookup/2
        ]).

%% layering API
-export([ gloenv/0
        , is_gloenv/1
        , nested/1
        ]).

-export_type([ synenv/0
             ]).

-define(gloenv, gloenv).
-define(env, env).
-define(nested, nested).

-type synenv() :: ?gloenv
                | {?env, es_env:env()}
                | {?nested, es_env:env()}.

%% API -------------------------------------------------------------------------

-spec empty() -> synenv().
empty() -> {?env, es_env:empty()}.

%% This rejects entering to es_gloenv.  Global bindings are translated to code
%% that updates the global environment as it executes.
-spec enter(synenv(), atom(), term()) -> synenv().
enter({?env, Env}, Var, Val) -> {?env, es_env:enter(Env, Var, Val)};
enter({?nested, Env}, Var, Val) -> {?nested, es_env:enter(Env, Var, Val)}.

%% Lookup an identifier in the syntax environment.  Returns:
%% none if the identifier is not bound at all
%% {value, false} if the identifier is bound as a variable
%% {value, Expander} if the identifier is bound as an expander
-spec lookup(synenv(), atom()) -> {value, term()} | none.
lookup(?gloenv, Var) -> lookup_gloenv(Var);
lookup({env, Env}, Var) -> es_env:lookup(Env, Var);
lookup({?nested, Env}, Var) ->
  case es_env:lookup(Env, Var) of
    {value, _} = Result -> Result;
    none -> lookup_gloenv(Var)
  end.

-spec gloenv() -> synenv().
gloenv() -> ?gloenv.

-spec is_gloenv(synenv()) -> boolean().
is_gloenv(?gloenv) -> true;
is_gloenv(_) -> false.

-spec nested(synenv()) -> synenv().
nested(?gloenv) -> {?nested, es_env:empty()};
nested(SynEnv) -> SynEnv.

%% Internals -------------------------------------------------------------------

lookup_gloenv(Var) ->
  case es_gloenv:lookup_expander(Var) of
    {value, _} = Result -> Result;
    none ->
      case es_gloenv:is_bound_var(Var) of
        true -> {value, false};
        false -> none
      end
  end.
