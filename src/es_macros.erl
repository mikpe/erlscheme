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
%%%
%%% TODO:
%%% - proper scoping / syntactic environment

-module(es_macros).

-export([ expand/1
        , init/0
        ]).

-type sexpr() :: term().

%% API -------------------------------------------------------------------------

-spec expand(sexpr()) -> sexpr().
expand(Sexpr) ->
  case Sexpr of
    [Hd | Tl] when is_atom(Hd) ->
      case get_syntax(Hd) of
        false ->
          case get_macro(Hd) of
            false ->
              [Hd | expand_list(Tl)];
            Expander ->
              expand(es_eval:do_apply(Expander, [Sexpr]))
          end;
        Expander ->
          es_eval:do_apply(Expander, [Sexpr])
      end;
    [_ | _] ->
      expand_list(Sexpr);
    _ ->
      Sexpr
  end.

-spec init() -> ok.
init() ->
  put_macro('macro', fun expand_macro/1),
  put_macro('compiler-syntax', fun expand_compiler_syntax/1),
  put_syntax('quote', fun expand_quote/1),
  put_syntax('set!', fun 'expand_set!'/1),
  put_syntax('cond', fun expand_cond/1),
  put_syntax('case', fun expand_case/1),
  put_syntax('lambda', fun expand_lambda/1),
  put_syntax('define', fun expand_define/1),
  put_syntax('letrec', fun expand_let_or_letrec/1),
  put_syntax('let*', fun 'expand_let*'/1),
  put_syntax('let', fun expand_let/1),
  put_macro('quasiquote', fun expand_quasiquote/1),
  ok.

%% Built-in macro and syntax expanders -----------------------------------------

-define(macro, '%macro').
-define(syntax, '%syntax').

%% (macro <name> <expander>)
expand_macro([_Macro, Name, Expander]) ->
  ['putprop', ['quote', Name], ['quote', ?macro], Expander].

%% (compiler-syntax <name> <expander>)
expand_compiler_syntax([_CompilerSyntax, Name, Expander]) ->
  ['putprop', ['quote', Name], ['quote', ?syntax], Expander].

%% (quote <datum>)
expand_quote([_Quote, _] = Sexpr) ->
  Sexpr.

%% (set! <var> <expr>)
'expand_set!'([Set, Var, Val]) ->
  [Set, Var, expand(Val)].

%% (cond <clause>+)
expand_cond([_Cond, Clause | Rest]) ->
  expand_cond(Clause, Rest).

expand_cond(['else' | Exprs], []) ->
  ['begin' | expand_list(Exprs)];
expand_cond([Test], Rest) ->
  %% TODO: generate (let ((<var> <test>)) (if <var> <var> <rest of cond>)) directly
  ['or', expand(Test), expand_cond_rest(Rest)];
expand_cond([Test | Exprs], Rest) ->
  %% TODO: this fails to handle (<test> => <expr>) which should become something
  %% like (let ((<var> <test>)) (if <var> (<expr> <var>) <rest of cond>)).
  ['if', expand(Test), ['begin' | expand_list(Exprs)], expand_cond_rest(Rest)].

expand_cond_rest([Clause | Rest]) -> expand_cond(Clause, Rest);
expand_cond_rest([]) -> expand_unspecified().

%% (case <expr> <clause>+)
expand_case([Case, Key, Clause | Rest]) ->
  [Case, expand(Key) | expand_case(Clause, Rest)].

expand_case(['else' | Exprs], []) ->
  ['else' | expand_list(Exprs)];
expand_case([Datums | Exprs], Rest) ->
  [[Datums | expand_list(Exprs)] | expand_case_rest(Rest)].

expand_case_rest([Clause | Rest]) -> expand_case(Clause , Rest);
expand_case_rest([]) -> expand_unspecified().

%% (lambda <formals> <body>+)
expand_lambda([Lambda, Formals | Body]) ->
  [Lambda, Formals | expand_body(Body)].

%% (define <var> <expr>)
%% (define (<var> <formals>*) <body>+)
expand_define([Define, [Var | Formals] | Body]) ->
  [Define, Var, ['lambda', Formals | expand_body(Body)]];
expand_define([Define, Var, Val]) ->
  [Define, Var, expand(Val)].

%% (let <bindings> <body>+)
%% (letrec <bindings> <body>+)
expand_let_or_letrec([LetOrLetRec, Bindings | Body]) ->
  [LetOrLetRec, lists:map(fun expand_let_binding/1, Bindings) | expand_body(Body)].

%% (let* <bindings> <body>+)
'expand_let*'([_LetStar, Bindings | Body]) ->
  'expand_let*'(Bindings, Body).

'expand_let*'([], Body) -> ['let', [] | expand_body(Body)];
'expand_let*'([Binding | Bindings], Body) ->
  ['let', [expand_let_binding(Binding)], 'expand_let*'(Bindings, Body)].

%% (let <bindings> <body>+)
%% (let <name> <bindings> <body>+)
expand_let([_Let, Name, Bindings | Body]) when is_atom(Name) ->
  Formals = lists:map(fun ([Var, _Init]) -> Var end, Bindings),
  Inits = lists:map(fun ([_Var, Init]) -> expand(Init) end, Bindings),
  Lambda = ['lambda', Formals | expand_body(Body)],
  ['letrec', [[Name, Lambda]], [Name | Inits]];
expand_let(Form) -> expand_let_or_letrec(Form).

%% Expander helpers ------------------------------------------------------------

expand_list(List) ->
  lists:map(fun expand/1, List).

%% expand (define ...) forms at the start of a body to (letrec ...)
expand_body(Body) ->
  expand_body_scan(expand_list(Body), []).

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

%% Sometimes we need to generate an unspecified value.
expand_unspecified() ->
  ['quote', es_datum:unspecified()].

expand_let_binding([Var, Expr]) ->
  [Var, expand(Expr)].

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

expand_quasiquote([_QQ, X]) ->
  descend_quasiquote(X, _Level = 0, _Return = fun finalize_quasiquote/2).

is_proper_list([_ | Tl]) -> is_proper_list(Tl);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

%% Lookup / Enter expander bindings --------------------------------------------

put_macro(Name, Expander) ->
  putprop(Name, ?macro, Expander).

put_syntax(Name, Expander) ->
  putprop(Name, ?syntax, Expander).

get_macro(Name) ->
  getprop(Name, ?macro).

get_syntax(Name) ->
  getprop(Name, ?syntax).

putprop(Name, Tag, Val) ->
  es_gloenv:insert(Name, Tag, Val).

getprop(Name, Tag) ->
  case es_gloenv:lookup(Name, Tag) of
    {value, Val} -> Val;
    none -> false
  end.
