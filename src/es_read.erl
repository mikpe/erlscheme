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
%%% es_read.erl
%%%
%%% An R7RS Reader for ErlScheme.
%%%
%%% Extensions:
%%% - allows bracketed list syntax from R6RS and other LISPs

-module(es_read).

-export([read/1]).

%%

read(LI) ->
  read_dispatch(LI, token(LI), true).

read_no_eof(LI) ->
  read_dispatch_no_eof(LI, token(LI)).

read_dispatch_no_eof(LI, Token) ->
  read_dispatch(LI, Token, false).

read_dispatch(LI, Token, EofOK) ->
  case Token of
    token_lparen ->
      read_list(LI, token_rparen, []);
    token_lbracket ->
      read_list(LI, token_rbracket, []);
    token_squote ->
      read_squote(LI);
    token_backquote ->
      read_backquote(LI);
    {token_number, Num} ->
      Num;
    token_hash_lparen ->
      read_vector(LI, []);
    {token_identifier, String} ->
      list_to_atom(String);
    token_true ->
      true;
    token_false ->
      false;
    token_comma_at ->
      read_comma_at(LI);
    token_comma ->
      read_comma(LI);
    {token_character, Ch} ->
      es_datum:integer_to_char(Ch);
    {token_string, String} ->
      es_datum:binary_to_string(list_to_binary(String));
    token_eof ->
      case EofOK of
	true -> es_datum:mk_eof_object();
	false -> erlang:throw(premature_eof)
      end;
    token_rparen ->
      erlang:throw(expected_datum_got_rparen);
    token_rbracket ->
      erlang:throw(expected_datum_got_rbracket);
    token_dot ->
      erlang:throw(expected_datum_got_dot)
      %% token_hash_semi is filtered out by token/1
  end.

read_squote(LI) ->
  ['quote', read_no_eof(LI)].

read_backquote(LI) ->
  ['quasiquote', read_no_eof(LI)].

read_comma(LI) ->
  ['unquote', read_no_eof(LI)].

read_comma_at(LI) ->
  ['unquote-splicing', read_no_eof(LI)].

read_list(LI, RightDelimiter, Acc) ->
  case token(LI) of
    RightDelimiter ->
      lists:reverse(Acc);
    token_dot ->
      X = read_no_eof(LI),
      case token(LI) of
	RightDelimiter ->
	  lists:reverse(Acc, X)
      end;
    Token ->
      X = read_dispatch_no_eof(LI, Token),
      read_list(LI, RightDelimiter, [X | Acc])
  end.

read_vector(LI, Acc) ->
  case token(LI) of
    token_rparen ->
      es_datum:list_to_vector(lists:reverse(Acc));
    Token ->
      X = read_dispatch_no_eof(LI, Token),
      read_vector(LI, [X | Acc])
  end.

%% return the next <token> from the <lexinput>
%% skip "#;<whitespace><datum>" comments
%% XXX: handle "#!{no-,}fold-case" directives here?

token(LI) ->
  case es_lexer:token(LI) of
    token_hash_semi ->
      read_no_eof(LI),
      token(LI);
    Token ->
      Token
  end.
