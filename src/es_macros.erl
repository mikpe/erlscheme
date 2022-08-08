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
%%% es_macros.erl
%%%
%%% Macro expansion for ErlScheme.
%%%
%%% - expand (<macro> ...) forms of <expr>
%%% - recognize special syntactic forms and propagate macro expansion to
%%%   nested <expr>s, while not expanding parts that aren't <expr>
%%% - optionally lower special syntactic forms to simpler core constructs

-module(es_macros).

%% compiler API
-export([ expand_toplevel/2
        , initial/0
        ]).

%% runtime API
-export([ enter_macro/2
        , enter_syntax/2
        ]).

-define(macro, '%macro').
-define(syntax, '%syntax').

-type sexpr() :: term().
-type expander() :: fun((sexpr(), synenv()) -> {sexpr(), synenv()}).
-type synenv() :: es_synenv:synenv(). % atom() -> expander()

%% API -------------------------------------------------------------------------

-spec expand_toplevel(sexpr(), synenv()) -> {sexpr(), synenv()}.
expand_toplevel(Sexpr, SynEnv) ->
  case Sexpr of
    [Hd | Tl] when is_atom(Hd) ->
      case find_expander(SynEnv, Hd) of
        false ->
          {[Hd | expand_list(Tl, SynEnv)], SynEnv};
        Expander ->
          Expander(Sexpr, SynEnv)
      end;
    [_ | _] ->
      {expand_list(Sexpr, SynEnv), SynEnv};
    _ ->
      {Sexpr, SynEnv}
  end.

-spec initial() -> [{atom(), expander()}].
initial() ->
  lists:map(
    fun ({Name, Type, Expander}) -> {Name, wrap_expander(Type, Expander)} end,
    [ {'begin', ?syntax, fun expand_begin/2}
    , {'case', ?syntax, fun expand_case/2}
    , {'compiler-syntax', ?syntax, fun expand_compiler_syntax/2}
    , {'cond', ?syntax, fun expand_cond/2}
    , {'define', ?syntax, fun expand_define/2}
    , {'lambda', ?syntax, fun expand_lambda/2}
    , {'let', ?syntax, fun expand_let/2}
    , {'let*', ?syntax, fun 'expand_let*'/2}
    , {'letrec', ?syntax, fun expand_let_or_letrec/2}
    , {'macro', ?syntax, fun expand_macro/2}
    , {'quasiquote', ?macro, fun expand_quasiquote/2}
    , {'quote', ?syntax, fun expand_quote/2}
    , {'set!', ?syntax, fun 'expand_set!'/2}
    , {'try', ?syntax, fun expand_try/2}
    ]).

-spec enter_macro(atom(), expander()) -> true.
enter_macro(Name, Expander) ->
  enter_expander(Name, wrap_expander(?macro, Expander)).

-spec enter_syntax(atom(), expander()) -> true.
enter_syntax(Name, Expander) ->
  enter_expander(Name, wrap_expander(?syntax, Expander)).

%% Built-in macro and syntax expanders -----------------------------------------

%% (macro <name> <expander>)
%% only valid at toplevel (repl, module, or body)
expand_macro([_Macro, Name, Expr], SynEnv) ->
  case es_synenv:is_gloenv(SynEnv) of
    true ->
      Expander = expand_expr(Expr, SynEnv),
      {[['quote', 'es_macros'], ':', ['quote', 'enter_macro'], ['quote', Name], Expander], SynEnv};
    false ->
      {Expander, _SynEnv} = es_eval:eval(Expr, SynEnv),
      {['begin'], bind_expander({Name, ?macro, Expander}, SynEnv)}
  end.

%% (compiler-syntax <name> <expander>)
%% only valid at toplevel (repl, module, or body)
expand_compiler_syntax([_CompilerSyntax, Name, Expr], SynEnv) ->
  case es_synenv:is_gloenv(SynEnv) of
    true ->
      Expander = expand_expr(Expr, SynEnv),
      {[['quote', 'es_macros'], ':', ['quote', 'enter_syntax'], ['quote', Name], Expander], SynEnv};
    false ->
      {Expander, _SynEnv} = es_eval:eval(Expr, SynEnv),
      {['begin'], bind_expander({Name, ?syntax, Expander}, SynEnv)}
  end.

%% (quote <datum>)
expand_quote([_Quote, _] = Sexpr, SynEnv) ->
  {Sexpr, SynEnv}.

%% (set! <var> <expr>)
'expand_set!'([Set, Var, Val], SynEnv) ->
  {[Set, Var, expand_expr(Val, SynEnv)], SynEnv}.

%% (cond <clause>+)
expand_cond([_Cond, Clause | Rest], SynEnv) ->
  {expand_cond(Clause, Rest, SynEnv), SynEnv}.

expand_cond(['else' | Exprs], [], SynEnv) ->
  %% TODO: R7RS states that (cond (else <exprs>)) reduces to (begin <exprs>), but
  %% I think that's wrong since (begin ..) is special in <toplevel> and <body>,
  %% allowing <exprs> to insert internal definitions in the surrounding context.
  ['begin' | expand_list(Exprs, SynEnv)];
expand_cond([Test], Rest, SynEnv) ->
  %% TODO: generate (let ((<var> <test>)) (if <var> <var> <rest of cond>)) directly
  ['or', expand_expr(Test, SynEnv), expand_cond_rest(Rest, SynEnv)];
expand_cond([Test | Exprs], Rest, SynEnv) ->
  %% TODO: this fails to handle (<test> => <expr>) which should become something
  %% like (let ((<var> <test>)) (if <var> (<expr> <var>) <rest of cond>)).
  ['if', expand_expr(Test, SynEnv), ['begin' | expand_list(Exprs, SynEnv)], expand_cond_rest(Rest, SynEnv)].

expand_cond_rest([Clause | Rest], SynEnv) -> expand_cond(Clause, Rest, SynEnv);
expand_cond_rest([], _SynEnv) -> expand_unspecified().

%% (case <expr> <clause>+)
expand_case([Case, Key, Clause | Rest], SynEnv) ->
  {[Case, expand_expr(Key, SynEnv) | expand_case(Clause, Rest, SynEnv)], SynEnv}.

expand_case(['else' | Exprs], [], SynEnv) ->
  ['else' | expand_list(Exprs, SynEnv)];
expand_case([Datums | Exprs], Rest, SynEnv) ->
  [[Datums | expand_list(Exprs, SynEnv)] | expand_case_rest(Rest, SynEnv)].

expand_case_rest([Clause | Rest], SynEnv) -> expand_case(Clause , Rest, SynEnv);
expand_case_rest([], _SynEnv) -> expand_unspecified().

%% (lambda <formals> <body>+)
expand_lambda([Lambda, Formals | Body], SynEnv) ->
  SynEnvBody = unbind_vars(Formals, nested(SynEnv)),
  {[Lambda, Formals | expand_body(Body, SynEnvBody)], SynEnv}.

%% (define <var> <expr>)
%% (define (<var> <formals>*) <body>+)
expand_define([Define, [Var | Formals] | Body], SynEnv) ->
  {Expanded, _SynEnv} = expand_lambda(['lambda', Formals | Body], SynEnv),
  {[Define, Var, Expanded], unbind_var(Var, SynEnv)};
expand_define([Define, Var, Val], SynEnv) ->
  {[Define, Var, expand_expr(Val, SynEnv)], unbind_var(Var, SynEnv)}.

%% (let <bindings> <body>+)
%% (letrec <bindings> <body>+)
expand_let_or_letrec([LetOrLetRec, Bindings | Body], SynEnv) ->
  Vars = lists:map(fun ([Var, _Init]) -> Var end, Bindings),
  SynEnvBody = unbind_vars(Vars, nested(SynEnv)),
  {[LetOrLetRec,
    lists:map(fun ([Var, Init]) -> [Var, expand_expr(Init, SynEnvBody)] end, Bindings) |
    expand_body(Body, SynEnvBody)],
   SynEnv}.

%% (let* <bindings> <body>+)
'expand_let*'([_LetStar, Bindings | Body], SynEnv) ->
  {'expand_let*'(Bindings, Body, SynEnv), SynEnv}.

'expand_let*'([], Body, SynEnv) -> ['let', [] | expand_body(Body, nested(SynEnv))];
'expand_let*'([[Var, Init] | Bindings], Body, SynEnv) ->
  ['let', [[Var, expand_expr(Init, SynEnv)]], 'expand_let*'(Bindings, Body, do_unbind_var(Var, nested(SynEnv)))].

%% (let <bindings> <body>+)
%% (let <name> <bindings> <body>+)
expand_let([_Let, Name, Bindings | Body], SynEnv) when is_atom(Name) ->
  Formals = lists:map(fun ([Var, _Init]) -> Var end, Bindings),
  Inits = lists:map(fun ([_Var, Init]) -> expand_expr(Init, SynEnv) end, Bindings),
  SynEnvBody = unbind_vars([Name | Formals], nested(SynEnv)),
  Lambda = ['lambda', Formals | expand_body(Body, SynEnvBody)],
  {[['letrec', [[Name, Lambda]], Name] | Inits], SynEnv};
expand_let(Form, SynEnv) -> expand_let_or_letrec(Form, SynEnv).

%% (try <expr> (of <var> <expr>+) (catch <var> <expr>+) (after <expr>+))
expand_try([Try, Expr0 | RestExpr], SynEnv) ->
  Expr = expand_expr(Expr0, SynEnv),
  {MaybeOf, RestOf} = expand_try_clause('of', RestExpr, SynEnv),
  {MaybeCatch, RestCatch} = expand_try_clause('catch', RestOf, SynEnv),
  After = expand_try_after(RestCatch, SynEnv),
  {[Try, Expr | (MaybeOf ++ (MaybeCatch ++ After))], SynEnv}.

expand_try_clause(Tag, [[Tag, Var | Exprs] | Rest], SynEnv) when is_atom(Var) ->
  {[[Tag, Var, ['begin' | expand_list(Exprs, do_unbind_var(Var, nested(SynEnv)))]]], Rest};
expand_try_clause(_Tag, Rest, _SynEnv) ->
  {[], Rest}.

expand_try_after([['after' | Exprs]], SynEnv) ->
  [['after', ['begin' | expand_list(Exprs, SynEnv)]]];
expand_try_after([], _SynEnv) ->
  [].

%% (begin <forms>..)
%% Begin is special since it essentially "disappears" in <toplevel> and <body>.
%% For those contexts it needs an expander that propagates SynEnv updates.
expand_begin([Begin | Forms], SynEnv) ->
  {NewForms, NewSynEnv} = expand_toplevel_forms(Forms, SynEnv, []),
  {[Begin | NewForms], NewSynEnv}.

%% Expander helpers ------------------------------------------------------------

expand_expr(Sexpr, SynEnv) ->
  {Expanded, _} = expand_toplevel(Sexpr, SynEnv),
  Expanded.

expand_list(List, SynEnv) ->
  lists:map(fun (Sexpr) -> expand_expr(Sexpr, SynEnv) end, List).

%% expand (define ...) forms at the start of a body to (letrec ...)
expand_body(Body, SynEnv) ->
  {ExpandedBody, _NewSynEnv} = expand_toplevel_forms(Body, SynEnv, []),
  expand_body_scan(ExpandedBody, []).

expand_body_scan([['define', Var, Val] | Body], Bindings) ->
  expand_body_scan(Body, [[Var, Val] | Bindings]);
expand_body_scan([['begin' | Rest] | Body], Bindings) ->
  %% a (begin ...) at the top-level of a <body> is essentially spliced into the <body>
  expand_body_scan(Rest ++ Body, Bindings);
expand_body_scan(Body = [_ | _], Bindings) ->
  %% No more (define ...), body must be non-empty, assemble the result
  case Bindings of
    [] -> Body;
    [_|_] -> ['letrec', Bindings | Body]
  end.

expand_toplevel_forms([Form | Forms], SynEnv, Acc) ->
  {Expanded, NewSynEnv} = expand_toplevel(Form, SynEnv),
  expand_toplevel_forms(Forms, NewSynEnv, [Expanded | Acc]);
expand_toplevel_forms(_Forms = [], SynEnv, Acc) ->
  {lists:reverse(Acc), SynEnv}.

%% Sometimes we need to generate an unspecified value.
expand_unspecified() ->
  ['quote', es_datum:unspecified()].

%% Quasiquote expander ---------------------------------------------------------
%%
%% Originally based on qquote.s from MIT C-Scheme:
%%
%%      Copyright (c) 1987 Massachusetts Institute of Technology
%%
%%      This material was developed by the Scheme project at the
%%      Massachusetts Institute of Technology, Department of
%%      Electrical Engineering and Computer Science.  Permission to
%%      copy this software, to redistribute it, and to use it for any
%%      purpose is granted, subject to the following restrictions and
%%      understandings.
%%
%%      1. Any copy made of this software must include this copyright
%%      notice in full.
%%
%%      2. Users of this software agree to make their best efforts (a)
%%      to return to the MIT Scheme project any improvements or
%%      extensions that they make, so that these may be included in
%%      future releases; and (b) to inform MIT of noteworthy uses of
%%      this software.
%%
%%      3. All materials developed as a consequence of the use of this
%%      software shall duly acknowledge such use, in accordance with
%%      the usual standards of acknowledging credit in academic
%%      research.
%%
%%      4. MIT has made no warrantee or representation that the
%%      operation of this software will be error-free, and MIT is
%%      under no obligation to provide any services, by way of
%%      maintenance, update, or otherwise.
%%
%%      5. In conjunction with products arising from the use of this
%%      material, there shall be no use of the name of the
%%      Massachusetts Institute of Technology nor of any adaptation
%%      thereof in any advertising, promotional, or sales literature
%%      without prior written consent from MIT in each case.

descend_quasiquote(X, Level, Return) ->
  case X of
    [_ | _] -> descend_quasiquote_pair(X, Level, Return);
    _ when is_tuple(X) -> descend_quasiquote_vector(X, Level, Return);
    _ -> Return('quote', X)
  end.

%% hoisted out of descend_quasiquote_pair/3 and eta-expanded
'descend_quasiquote_pair*'([CarX | CdrX] = X, Level, Return) ->
  descend_quasiquote(CarX, Level,
    fun (CarMode, CarArg) ->
      descend_quasiquote(CdrX, Level,
        fun (CdrMode, CdrArg) ->
          if CarMode =:= 'quote' andalso CdrMode =:= 'quote' ->
               Return('quote', X);
             CarMode =:= 'unquote-splicing' ->
               if CdrMode =:= 'quote' andalso CdrArg =:= [] ->
                    Return('unquote', CarArg);
                  true ->
                    Return(system('append'),
                           [CarArg, finalize_quasiquote(CdrMode, CdrArg)])
               end;
             CdrMode =:= 'quote' andalso CdrArg =:= [] ->
               Return('list',
                      [CarArg, finalize_quasiquote(CarMode, CarArg)]);
             CdrMode =:= 'quote' ->
               case is_proper_list(CdrArg) of
                 true ->
                   Return('list',
                          [finalize_quasiquote(CarMode, CarArg) |
                           lists:map(fun (El) -> finalize_quasiquote('quote', El) end, CdrArg)]);
                 false -> % same as the default clause below
                   Return('cons',
                          [finalize_quasiquote(CarMode, CarArg),
                           finalize_quasiquote(CdrMode, CdrArg)])
               end;
             CdrMode =:= 'list' orelse CdrMode =:= 'cons' ->
               Return(CdrMode, [finalize_quasiquote(CarMode, CarArg) | CdrArg]);
             true ->
               Return('cons',
                      [finalize_quasiquote(CarMode, CarArg),
                       finalize_quasiquote(CdrMode, CdrArg)])
          end
        end)
    end).

descend_quasiquote_pair([CarX | _] = X, Level, Return) ->
  if CarX =:= 'quasiquote' ->
       'descend_quasiquote_pair*'(X, Level + 1, Return);
     CarX =:= 'unquote' orelse CarX =:= 'unquote-splicing' ->
       if Level =:= 0 -> Return(CarX, hd(tl(X)));
          true -> 'descend_quasiquote_pair*'(X, Level - 1, Return)
       end;
     true ->
       'descend_quasiquote_pair*'(X, Level, Return)
  end.

descend_quasiquote_vector(X, Level, Return) ->
  descend_quasiquote(tuple_to_list(X), Level,
    fun (Mode, Arg) ->
      case Mode of
        'quote' ->
          Return('quote', X);
        'list' ->
          Return(system('vector'), Arg);
        _ ->
          Return(system('list->vector'),
                 [finalize_quasiquote(Mode, Arg)])
      end
    end).

finalize_quasiquote(Mode, Arg) ->
  case Mode of
    'quote' -> ['quote', Arg];
    'unquote' -> Arg;
    'unquote-splicing' -> error({",@ in invalid context", Arg});
    'list' -> [system('list') | Arg];
    'cons' ->
      case Arg of
        [_, _] -> [system('cons') | Arg]; % (= (length arg) 2)
        _ -> 'finalize_cons*'(Arg)
      end;
    _ -> [Mode | Arg]
  end.

%% C-Scheme synthesized (cons* <arg1> ... <argN> <argN+1>) here.
%% We synthesize (append (list <arg1> ... <argN>) <argN+1>) to avoid
%% variadic procedures (list is a built-in constructor).
'finalize_cons*'([Arg]) -> Arg;
'finalize_cons*'(Args) ->
  RevArgs = lists:reverse(Args),
  [Last | RevButLast] = RevArgs,
  ButLast = lists:reverse(RevButLast),
  [system('append'), [system('list') | ButLast], Last].

system(Name) ->
  %% TODO: generate the "system" definition of a standard procedure
  %% (append, vector, list->vector, list, or cons).  This needs to
  %% work even if the user has rebound that identifier.
  Name.

expand_quasiquote([_QQ, X], SynEnv) ->
  {descend_quasiquote(X, _Level = 0, _Return = fun finalize_quasiquote/2), SynEnv}.

is_proper_list([_ | Tl]) -> is_proper_list(Tl);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

%% Syntax Environment Operations -----------------------------------------------

enter_expander(Name, Expander) ->
  es_gloenv:enter_expander(Name, Expander).

find_expander(SynEnv, Name) ->
  case es_synenv:lookup(SynEnv, Name) of
    {value, Expander} when Expander =/= false -> Expander;
    _ -> false % none (unbound) or {value, false} (shadowed)
  end.

bind_expander({Name, Type, Expander}, SynEnv) ->
  es_synenv:enter(SynEnv, Name, wrap_expander(Type, Expander)).

wrap_expander(?syntax, Expander) -> Expander;
wrap_expander(?macro, Expander) ->
  fun (Sexpr, SynEnv) ->
    {NewSexpr, NewSynEnv} = Expander(Sexpr, SynEnv),
    expand_toplevel(NewSexpr, NewSynEnv)
  end.

unbind_vars(Vars, SynEnv) ->
  false = es_synenv:is_gloenv(SynEnv), % assert
  lists:foldl(fun do_unbind_var/2, SynEnv, Vars).

unbind_var(Var, SynEnv) ->
  case es_synenv:is_gloenv(SynEnv) of
    true ->
      SynEnv; % toplevel (define ..), nothing for us to do
    false ->
      do_unbind_var(Var, SynEnv)
  end.

do_unbind_var(Var, SynEnv) ->
  %% Removing a global binding within a local scope doesn't work, so instead
  %% we "unbind" a macro by rebinding it to a non-expander, currently 'false'.
  es_synenv:enter(SynEnv, Var, false).

nested(SynEnv) ->
  es_synenv:nested(SynEnv).
