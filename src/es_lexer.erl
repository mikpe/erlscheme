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
%%% es_lexer.erl
%%%
%%% R7RS-like Lexical Analyzer for ErlScheme.
%%%
%%% Limitations:
%%% - #!fold-case and #!no-fold-case are not yet implemented
%%% - "#u8(" for the <bytevector> syntax is not yet implemented
%%% - only supports the ASCII character set, embedded in 8-bit characters
%%% - the polar, rectangular, and rational number syntaxes are not supported
%%%
%%% Extensions:
%%% - recognizes [ and ] for R6RS bracketed list syntax (and other LISPs before that)
%%% - : and / are delimiters which read as single-character symbols

-module(es_lexer).

-export([ string_to_number/2
        , token/1
        ]).

-type token() :: atom() | {atom(), term()}.

%% API -------------------------------------------------------------------------

-spec token(es_lexinput:lexinput()) -> token().
token(LI) ->
  case skip_intertoken_space(LI) of
    -1 ->
      token_eof;
    40 -> % left-paren, messes up erlang-mode :-(
      token_lparen;
    41 -> % right-paren, messes up erlang-mode :-(
      token_rparen;
    91 -> % left-bracket, messes up erlang-mode :-(
      token_lbracket;
    93 -> % right-bracket, messes up erlang-mode :-(
      token_rbracket;
    39 -> % single-quote, messes up erlang-mode :-(
      token_squote;
    $` ->
      token_backquote;
    $, ->
      scan_comma(LI);
    34 -> % double-quote, messes up erlang-mode :-(
      scan_string(LI);
    $# ->
      scan_hash(LI);
    46 -> % dot, messes up erlang-mode :-(
      scan_pi_dot(LI, [46], false);
    $+ ->
      scan_pi_sign(LI, $+, false);
    $- ->
      scan_pi_sign(LI, $-, true);
    $| ->
      scan_vertical_identifier(LI);
    $: -> % ErlScheme extension
      {token_identifier, ":"};
    $/ -> % ErlScheme extension
      {token_identifier, "/"};
    Ch ->
      case es_ctype:char_is_initial(Ch) of
        true ->
          scan_simple_identifier(LI, [Ch]);
        false ->
          Val = es_ctype:char_value(Ch),
          if Val < 10 ->
              case decimal_q3(LI, Val, false, false) of
                Num when is_number(Num) ->
                  {token_number, Num};
                false ->
                  erlang:throw(invalid_number)
              end;
             true ->
              erlang:throw({invalid_character, Ch})
          end
      end
  end.

-spec string_to_number(string(), integer()) -> number() | false.
string_to_number(String, Radix) ->
  Port = es_raw_port:open_input_string(String),
  try
    LI = es_lexinput:open(Port, []),
    try
      case number_q0(LI, Radix) of
        Num when is_number(Num) ->
          case es_lexinput:peek_char(LI) of
            -1 ->
              Num;
            _ ->
              false
          end;
        false ->
          false
      end
    after
      es_lexinput:close(LI)
    end
  after
    es_raw_port:close_input_port(Port)
  end.

%% Internals -------------------------------------------------------------------

%% scan_hash: read what follows a '#'
%% (a <boolean>, <character>, <vector> start, <bytevector> start, or <number>)
%% #| ... |# comments have already been eliminated by skip_intertoken_space.

scan_hash(LI) ->
  Ch = es_lexinput:peek_char(LI),
  case scan_hash_norm(Ch) of
    $\\ ->
      es_lexinput:read_char(LI),
      scan_character(LI);
    40 -> % left-paren, messes up erlang-mode :-(
      es_lexinput:read_char(LI),
      token_hash_lparen;
    $; ->
      es_lexinput:read_char(LI),
      token_hash_semi;
%    $! -> % TODO: NYI: start of <directive>
%    $U -> % TODO: NYI: start of <bytevector>
    $T ->
      es_lexinput:read_char(LI),
      scan_boolean(LI, Ch);
    $F ->
      es_lexinput:read_char(LI),
      scan_boolean(LI, Ch);
    _ ->
      case number_q1(LI, 10) of
        Num when is_number(Num) ->
          {token_number, Num};
        false ->
          erlang:throw(invalid_number)
      end
  end.

scan_hash_norm(Ch) -> % normalize case of relevant characters
  case Ch of
    $t -> $T;
    $f -> $F;
    _  -> Ch
  end.

scan_boolean(LI, Ch) ->
  {token_identifier, String} = scan_simple_identifier(LI, [Ch]),
  String2 = string:to_upper(String),
  if String2 == "T"; String2 == "TRUE" -> % TODO: #TRUE seems to be new in R7RS
      token_true;
     String2 == "F"; String2 == "FALSE" -> % TODO: #FALSE seems to be new in R7RS
      token_false;
     true ->
      erlang:throw({invalid_boolean, String})
  end.

%% <peculiar identifier>
%%      -> <explicit sign>
%%       | <explicit sign> <sign subsequent> <subsequent>*
%%       | <explicit sign> "." <dot subsequent> <subsequent>*
%%       | "." <dot subsequent> <subsequent>*
%% <dot subsequent> (TODO: == <subsequent> \ <digit>)
%%      -> <sign subsequent>
%%       | "."
%% <sign subsequent> (TODO: == <subsequent> \ <digit> \ ".")
%%      -> <initial>
%%       | <explicit sign>
%%       | "@"
%% <explicit sign>
%%      -> "+" | "-"

scan_pi_sign(LI, Ch0, IsNegative) ->
  Ch1 = es_lexinput:peek_char(LI),
  case es_ctype:char_is_subsequent(Ch1) of
    true ->
      case es_ctype:char_is_numeric(Ch1) of
        true ->
          case decimal_q1(LI, false, IsNegative) of
            Num when is_number(Num) ->
              {token_number, Num};
            false ->
              erlang:throw(invalid_number)
          end;
        false ->
          if Ch1 =:= 46 -> % dot, messes up erlang-mode :-(
              scan_pi_dot(LI, [Ch1, Ch0], IsNegative);
             true -> % <sign subsequent>
              es_lexinput:read_char(LI),
              scan_simple_identifier(LI, [Ch1, Ch0])
          end
      end;
    false ->
      case es_ctype:char_is_delimiter(Ch1) of
        true ->
          {token_identifier, [Ch0]};
        false ->
          erlang:throw(invalid_identifier)
      end
  end.

scan_pi_dot(LI, Acc, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  case es_ctype:char_is_subsequent(Ch) of
    true ->
      case es_ctype:char_is_numeric(Ch) of
        true ->
          case decimal_q2(LI, IsNegative) of
            Num when is_number(Num) ->
              {token_number, Num};
            false ->
              erlang:throw(invalid_number)
          end;
        false -> % <dot subsequent>
          es_lexinput:read_char(LI),
          scan_simple_identifier(LI, [Ch | Acc])
      end;
    false ->
      case es_ctype:char_is_delimiter(Ch) of
        true ->
          case Acc of % detect if this is "." (Ok) or "[+-]." (invalid)
            [46] ->
              token_dot;
            [_, 46] ->
              erlang:throw(invalid_identifier)
          end;
        false ->
          erlang:throw(invalid_identifier)
      end
  end.

%% scan_simple_identifier: scan <subsequent>* after seeing <initial>

scan_simple_identifier(LI, Acc) ->
  Ch = es_lexinput:peek_char(LI),
  case es_ctype:char_is_subsequent(Ch) of
    true ->
      es_lexinput:read_char(LI),
      scan_simple_identifier(LI, [Ch | Acc]);
    false ->
      case es_ctype:char_is_delimiter(Ch) of
        true ->
          {token_identifier, lists:reverse(Acc)};
        false ->
          erlang:throw({expected_delimiter, Ch})
      end
  end.

%% scan_vertical_identifier

scan_vertical_identifier(LI) ->
  scan_vi(LI, []).

scan_vi(LI, Acc) ->
  Ch = es_lexinput:read_char(LI),
  case Ch of
    -1 ->
      erlang:throw(premature_eof);
    $| ->
      {token_identifier, lists:reverse(Acc)};
    $\\ ->
      scan_vi_backslash(LI, Acc);
    Ch ->
      scan_vi(LI, [Ch | Acc])
  end.

scan_vi_backslash(LI, Acc) ->
  case scan_inline_escape(LI) of
    Val when is_number(Val) ->
      scan_vi(LI, [Val | Acc]);
    {error, Ch} ->
      erlang:throw({invalid_character, Ch})
  end.

%% seen ",", check for ",@"

scan_comma(LI) ->
  case es_lexinput:peek_char(LI) of
    $@ ->
      es_lexinput:read_char(LI),
      token_comma_at;
    _ ->
      token_comma
  end.

%% scan_character: read a character literal after seeing #\

scan_character(LI) ->
  Ch1 = es_lexinput:read_char(LI),
  Ch2 = es_lexinput:peek_char(LI),
  case es_ctype:char_is_delimiter(Ch2) of
    true ->
      {token_character, Ch1};
    false ->
      if Ch1 =:= $x; Ch1 =:= $X ->
          {token_character, scan_hex_scalar_value(LI, true)};
         true ->
          {token_identifier, String} = scan_simple_identifier(LI, [Ch1]),
          {token_character,
           case String of % note that case is significant in <character name>
             "alarm" -> 7;
             "backspace" -> 8;
             "delete" -> 127;
             "escape" -> 27;
             "newline" -> 10;
             "null" -> 0;
             "return" -> 13;
             "space" -> 32;
             "tab" -> 9;
             _ -> erlang:throw({invalid_character_name, String})
           end}
      end
  end.

%% scan_string: read a string literal after seeing the first "

scan_string(LI) ->
  scan_string(LI, []).

scan_string(LI, Acc) ->
  scan_string(es_lexinput:read_char(LI), LI, Acc).

scan_string(Ch, LI, Acc) ->
  case Ch of
    -1 ->
      erlang:throw(premature_eof);
    34 -> % double-quote, messes up erlang-mode :-(
      {token_string, lists:reverse(Acc)};
    $\\ ->
      scan_string_backslash(LI, Acc);
    Ch ->
      scan_string(LI, [Ch | Acc])
  end.

scan_string_backslash(LI, Acc) ->
  case scan_inline_escape(LI) of
    Val when is_number(Val) ->
      scan_string(LI, [Val | Acc]);
    {error, Ch} ->
      case es_ctype:char_is_whitespace(Ch) of
        true ->
          scan_string_gap1(LI, Acc);
        false ->
          erlang:throw({invalid_character, Ch})
      end
  end.

%% Shared function for handling the common cases of <string element> and
%% <symbol element>, after seeing a backslash.  Apart from <string gap>,
%% the differences between <string element> and <symbol element> in R7RS
%% 7.1.1 seem like documentation mistakes, so we deliberately allow the
%% same escape sequences for both contexts.  <string gap> is handled by
%% tagging and returning any unrecognized character, letting the context
%% determine whether that character is valid or not.

scan_inline_escape(LI) -> % return Octet or {error, Ch}
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    $a -> % alarm
      7;
    $b -> % backspace
      8;
    $t -> % tab
      9;
    $n -> % linefeed
      10;
    $r -> % carriage return
      13;
    %% This case is missing from R7RS 7.1.1 <symbol element>.
    %% However, 2.1 "Identifiers" states that <vertical identifier>
    %% allows the same escape sequences that are allowed in strings.
    34 -> % double-quote, messes up erlang-mode :-(
      34;
    %% This case is missing from R7RS 7.1.1 <symbol element>.
    %% However, 2.1 "Identifiers" states that <vertical identifier>
    %% allows the same escape sequences that are allowed in strings.
    $\\ ->
      92;
    %% This case is missing from R7RS 7.1.1 <string element>.
    %% However, 6.7 "Strings" lists it.
    $| -> % vertical line
      124;
    $x -> % inline hex escape
      scan_hex_scalar_value(LI, ';');
    Ch ->
      {error, Ch}
  end.

scan_hex_scalar_value(LI, Delimiter) ->
  Ch = es_lexinput:read_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < 16 ->
      scan_hex_scalar_value(LI, Delimiter, Val);
     true ->
      erlang:throw({expected_hex_digit, Ch})
  end.

scan_hex_scalar_value(LI, Delimiter, Num) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < 16 ->
      es_lexinput:read_char(LI),
      scan_hex_scalar_value(LI, Delimiter, (Num * 16) + Val);
     true ->
      case Delimiter of
        true ->
          case es_ctype:char_is_delimiter(Ch) of
            true ->
              [];
            false ->
              erlang:throw({expected_delimiter, Ch})
          end;
        ';' ->
          case Ch =:= ';' of
            true ->
              es_lexinput:read_char(LI);
            false ->
              erlang:throw({expected_semicolon, Ch})
          end
      end,
      if Val < 256 -> % TODO: 8-bit characters assumption
          Val;
         true ->
          erlang:throw({out_of_range_character_value, Val})
      end
  end.

scan_string_gap1(LI, Acc) -> % before <line ending>
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    10 -> % linefeed
      scan_string_gap3(LI, Acc);
    13 -> % carriage return
      scan_string_gap2(LI, Acc);
    Ch ->
      case es_ctype:char_is_whitespace(Ch) of
        true ->
          scan_string_gap1(LI, Acc);
        false ->
          erlang:throw({invalid_character, Ch})
      end
  end.

scan_string_gap2(LI, Acc) -> % after <carriage return>
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    10 -> % linefeed
      scan_string_gap3(LI, Acc);
    Ch ->
      scan_string_gap3(Ch, LI, Acc)
  end.

scan_string_gap3(LI, Acc) -> % after <line ending>
  scan_string_gap3(es_lexinput:read_char(LI), LI, Acc).

scan_string_gap3(Ch, LI, Acc) -> % after <line ending>
  case Ch of
    -1 ->
      erlang:throw(premature_eof);
    _ ->
      case es_ctype:char_is_whitespace(Ch) of
        true ->
          scan_string_gap3(LI, Acc);
        false ->
          scan_string(Ch, LI, Acc)
      end
  end.

%% The following implements a scanner for numbers:
%%
%% <num R>      -> <prefix R> <complex R>
%% <complex R>  -> <real R>
%%      [remaining alternatives omitted]
%% <real R>     -> <sign> <ureal R>
%%      [remaining alternatives omitted]
%% <ureal R>    -> <uinteger  R>
%%               | <decimal R>
%%      [remaining alternatives omitted]
%% <decimal 10> -> <uinteger 10> <suffix>
%%               | "." <digit 10>+ <suffix>
%%               | <digit 10>+ "." <digit 10>* <suffix>
%% <uinteger R> -> <digit R>+
%% <prefix R>   -> <radix R> <exactness>
%%               | <exactness> <radix R>
%% <suffix>     -> <empty>
%%               | <exponent marker> <sign> <digit 10>+
%% <exponent marker> -> "e"
%% <sign>       -> <empty> | "+" | "-"
%% <exactness>  -> <empty> | "#i" | "#e"
%% <radix 2>    -> "#b"
%% <radix 8>    -> "#o"
%% <radix 10>   -> <empty> | "#d"
%% <radix 16>   -> "#x"
%% <digit 2>    -> [0-1]
%% <digit 8>    -> [0-7]
%% <digit 10>   -> [0-9]
%% <digit 16>   -> [0-9a-f]
%%
%% Notes:
%% 1) All alphabetic characters used in these rules may appear
%%    in either upper or lower case.
%% 2) Returns false on failure.

number_q0(LI, Radix) ->
  Ch = es_lexinput:peek_char(LI),
  case Ch of
    $# ->
      es_lexinput:read_char(LI),
      number_q1(LI, Radix);
    $+ ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, false, false);
    $- ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, false, true);
    _ ->
      Val = es_ctype:char_value(Ch),
      if Val < Radix ->
          es_lexinput:read_char(LI),
          choose_decimal_q3_or_integer_q2(LI, Radix, Val, false);
         true ->
          false
      end
  end.

number_q1(LI, Radix) -> % after "#"
  Ch = es_lexinput:peek_char(LI),
  case number_q1_norm(Ch) of
    $B ->
      es_lexinput:read_char(LI),
      number_q2(LI, 2);
    $O ->
      es_lexinput:read_char(LI),
      number_q2(LI, 8);
    $D ->
      es_lexinput:read_char(LI),
      number_q2(LI, 10);
    $X ->
      es_lexinput:read_char(LI),
      number_q2(LI, 16);
    $E ->
      es_lexinput:read_char(LI),
      number_q4(LI, Radix, false);
    $I ->
      es_lexinput:read_char(LI),
      number_q4(LI, Radix, true);
    _ ->
      false
  end.

number_q1_norm(Ch) -> % normalize case of relevant characters
  case Ch of
    $b -> $B;
    $o -> $O;
    $d -> $D;
    $x -> $X;
    $e -> $E;
    $i -> $I;
    _  -> Ch
  end.

number_q2(LI, Radix) -> % after "#{B,O,D,X}"
  case es_lexinput:peek_char(LI) of
    $# ->
      es_lexinput:read_char(LI),
      number_q3(LI, Radix);
    $+ ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, false, false);
    $- ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, false, true);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < Radix ->
          es_lexinput:read_char(LI),
          choose_decimal_q3_or_integer_q2(LI, Radix, Val, false);
         true ->
          false
      end
  end.

number_q3(LI, Radix) -> % after "#{B,O,D,X}#"
  Ch = es_lexinput:peek_char(LI),
  case number_q3_norm(Ch) of
    $E ->
      es_lexinput:read_char(LI),
      choose_decimal_q0_or_integer_q0(LI, Radix, false);
    $I ->
      es_lexinput:read_char(LI),
      choose_decimal_q0_or_integer_q0(LI, Radix, true);
    _ ->
      false
  end.

number_q3_norm(Ch) -> % normalize case of relevant characters
  case Ch of
    $e -> $E;
    $i -> $I;
    _  -> Ch
  end.

number_q4(LI, Radix, IsInexact) -> % after "#{E,I}"
  case es_lexinput:peek_char(LI) of
    $# ->
      es_lexinput:read_char(LI),
      number_q5(LI, IsInexact);
    $+ ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, IsInexact, false);
    $- ->
      es_lexinput:read_char(LI),
      choose_decimal_q1_or_integer_q1(LI, Radix, IsInexact, true);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < Radix ->
          es_lexinput:read_char(LI),
          choose_decimal_q3_or_integer_q2(LI, Radix, Val, IsInexact);
         true ->
          false
      end
  end.

number_q5(LI, IsInexact) -> % after "#{E,I}#"
  Ch = es_lexinput:peek_char(LI),
  case number_q5_norm(Ch) of
    $B ->
      es_lexinput:read_char(LI),
      integer_q0(LI, 2, IsInexact);
    $O ->
      es_lexinput:read_char(LI),
      integer_q0(LI, 8, IsInexact);
    $X ->
      es_lexinput:read_char(LI),
      integer_q0(LI, 16, IsInexact);
    $D ->
      es_lexinput:read_char(LI),
      decimal_q0(LI, IsInexact);
    _ ->
      false
  end.

number_q5_norm(Ch) -> % normalize case of relevant characters
  case Ch of
    $b -> $B;
    $o -> $O;
    $x -> $X;
    $d -> $D;
    _  -> Ch
  end.

choose_decimal_q0_or_integer_q0(LI, Radix, IsInexact) -> % after "#[BODX]#[EI]"
  if Radix =:= 10 ->
      decimal_q0(LI, IsInexact);
     true ->
      integer_q0(LI, Radix, IsInexact)
  end.

choose_decimal_q1_or_integer_q1(LI, Radix, IsInexact, IsNegative) -> % after (#[BODXEI])?[+-]
  if Radix =:= 10 ->
      decimal_q1(LI, IsInexact, IsNegative);
     true ->
      integer_q1(LI, Radix, IsInexact, IsNegative)
  end.

choose_decimal_q3_or_integer_q2(LI, Radix, Num, IsInexact) -> % after (#[BODXEI]?)<digit R>
  if Radix =:= 10 ->
      decimal_q3(LI, Num, IsInexact, false);
     true ->
      integer_q2(LI, Radix, Num, IsInexact, false)
  end.

%% Scan <sign><uinteger {2,8,16}>

integer_q0(LI, Radix, IsInexact) ->
  case es_lexinput:peek_char(LI) of
    $+ ->
      es_lexinput:read_char(LI),
      integer_q1(LI, Radix, IsInexact, false);
    $- ->
      es_lexinput:read_char(LI),
      integer_q1(LI, Radix, IsInexact, true);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < Radix ->
          es_lexinput:read_char(LI),
          integer_q2(LI, Radix, Val, IsInexact, false);
         true ->
          false
      end
  end.

integer_q1(LI, Radix, IsInexact, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < Radix ->
      es_lexinput:read_char(LI),
      Num = if IsNegative -> -Val; true -> Val end,
      integer_q2(LI, Radix, Num, IsInexact, IsNegative);
     true ->
      false
  end.

integer_q2(LI, Radix, Num, IsInexact, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < Radix ->
      es_lexinput:read_char(LI),
      Num2 = Num * Radix,
      Num3 = if IsNegative -> Num2 - Val; true -> Num2 + Val end,
      integer_q2(LI, Radix, Num3, IsInexact, IsNegative);
     true ->
      %% R5RS allowed "#" to be used as a zero that also
      %% forced the resulting number to become inexact, e.g.
      %% 15## became 1500.0.  This is not supported in R7RS.
      case es_ctype:char_is_delimiter(Ch) of
        true ->
          if IsInexact ->
              float(Num);
             true ->
              Num
          end;
        false ->
          false
      end
  end.

%% Scan <sign><decimal 10>

decimal_q0(LI, IsInexact) ->
  case es_lexinput:peek_char(LI) of
    46 -> % dot, messes up erlang-mode :-(
      es_lexinput:read_char(LI),
      decimal_q2(LI, false);
    $+ ->
      es_lexinput:read_char(LI),
      decimal_q1(LI, IsInexact, false);
    $- ->
      es_lexinput:read_char(LI),
      decimal_q1(LI, IsInexact, true);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < 10 ->
          es_lexinput:read_char(LI),
          decimal_q3(LI, Val, IsInexact, false);
         true ->
          false
      end
  end.

decimal_q1(LI, IsInexact, IsNegative) ->
  case es_lexinput:peek_char(LI) of
    46 -> % dot, messes up erlang-mode :-(
      decimal_q2(LI, IsNegative);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < 10 ->
          es_lexinput:read_char(LI),
          Num = if IsNegative -> -Val; true -> Val end,
          decimal_q3(LI, Num, IsInexact, IsNegative);
         true ->
          false
      end
  end.

decimal_q2(LI, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < 10 ->
      es_lexinput:read_char(LI),
      Num = if IsNegative -> -Val; true -> Val end,
      decimal_q4(LI, Num, -1, IsNegative);
     true ->
      false
  end.

decimal_q3(LI, Num, IsInexact, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  if Ch =:= 46 -> % dot, messes up erlang-mode :-(
      es_lexinput:read_char(LI),
      decimal_q4(LI, Num, 0, IsNegative);
     %% R5RS allowed "#" to be used as a zero that also
     %% forced the resulting number to become inexact, e.g.
     %% 15## became 1500.0.  This is not supported in R7RS.
     Ch =:= $E; Ch =:= $e ->
      es_lexinput:read_char(LI),
      decimal_q7(LI, Num, 0);
     true ->
      Val = es_ctype:char_value(Ch),
      if Val < 10 ->
          es_lexinput:read_char(LI),
          Num2 = Num * 10,
          Num3 = if IsNegative -> Num2 - Val; true -> Num2 + Val end,
          decimal_q3(LI, Num3, IsInexact, IsNegative);
         true ->
          if IsInexact ->
              float(Num);
             true ->
              Num
          end
      end
  end.

decimal_q4(LI, Num, Shift, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  if Ch =:= $E; Ch =:= $e ->
      es_lexinput:read_char(LI),
      decimal_q7(LI, Num, Shift);
     %% R5RS allowed "#" to be used as a zero that also
     %% forced the resulting number to become inexact, e.g.
     %% 15## became 1500.0.  This is not supported in R7RS.
     true ->
      Val = es_ctype:char_value(Ch),
      if Val < 10 ->
          es_lexinput:read_char(LI),
          Num2 = Num * 10,
          Num3 = if IsNegative -> Num2 - Val; true -> Num2 + Val end,
          decimal_q4(LI, Num3, Shift - 1, IsNegative);
         true ->
          decimal_scale(Num, 0.10, -Shift)
      end
  end.

decimal_q7(LI, Num, Shift) ->
  case es_lexinput:peek_char(LI) of
    $+ ->
      es_lexinput:read_char(LI),
      decimal_q8(LI, Num, Shift, false);
    $- ->
      es_lexinput:read_char(LI),
      decimal_q8(LI, Num, Shift, true);
    Ch ->
      Val = es_ctype:char_value(Ch),
      if Val < 10 ->
          es_lexinput:read_char(LI),
          decimal_q9(LI, Num, Shift, false, Val);
         true ->
          false
      end
  end.

decimal_q8(LI, Num, Shift, IsNegative) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < 10 ->
      es_lexinput:read_char(LI),
      Val2 = if IsNegative -> -Val; true -> Val end,
      decimal_q9(LI, Num, Shift, IsNegative, Val2);
     true ->
      false
  end.

decimal_q9(LI, Num, Shift, IsNegative, Exp) ->
  Ch = es_lexinput:peek_char(LI),
  Val = es_ctype:char_value(Ch),
  if Val < 10 ->
      es_lexinput:read_char(LI),
      Exp2 = Exp * 10,
      Exp3 = if IsNegative -> Exp2 - Val; true -> Exp2 + Val end,
      decimal_q9(LI, Num, Shift, IsNegative, Exp3);
     true ->
      Shift2 = Shift + Exp,
      if Shift2 < 0 ->
          decimal_scale(Num, 0.10, -Shift2);
         true ->
          decimal_scale(Num, 10.0, Shift2)
      end
  end.

decimal_scale(Num, Scale, Shift) ->
  if Shift > 0 ->
      decimal_scale(Num * Scale, Scale, Shift - 1);
     true ->
      float(Num)
  end.

%% skip_intertoken_space: Skip over whitespace, ";..\n" comments and properly
%% nested "#|..|#" block comments.  Return first non-blank character or EOF.
%% May generate an error if there is a premature EOF in a block comment.
%% "#;<whitespace><datum>" comments are handled in the parser.

skip_intertoken_space(LI) ->
  case es_lexinput:read_char(LI) of
    -1 ->
      -1;
    $; ->
      skip_line_comment(LI);
    $# ->
      skip_hash(LI);
    Ch ->
      case es_ctype:char_is_whitespace(Ch) of
        true ->
          skip_intertoken_space(LI);
        false ->
          Ch
      end
  end.

skip_line_comment(LI) ->
  case es_lexinput:read_char(LI) of
    -1 ->
      -1; % permit ";...<EOF>" without \n before the <EOF>
    $\n ->
      skip_intertoken_space(LI);
    $\r ->
      skip_intertoken_space(LI);
    _ ->
      skip_line_comment(LI)
  end.

skip_hash(LI) ->
  case es_lexinput:peek_char(LI) of
    $| ->
      es_lexinput:read_char(LI),
      skip_block(LI, 1);
    _ ->
      $#
  end.

skip_block(LI, Level) ->
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    $# ->
      skip_block_hash(LI, Level);
    $| ->
      skip_block_bar(LI, Level);
    _ ->
      skip_block(LI, Level)
  end.

skip_block_hash(LI, Level) ->
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    $# ->
      skip_block_hash(LI, Level);
    $| ->
      skip_block(LI, Level + 1);
    _ ->
      skip_block(LI, Level)
  end.

skip_block_bar(LI, Level) ->
  case es_lexinput:read_char(LI) of
    -1 ->
      erlang:throw(premature_eof);
    $| ->
      skip_block_bar(LI, Level);
    $# ->
      if Level > 1 ->
          skip_block(LI, Level - 1);
         true ->
          skip_intertoken_space(LI)
      end;
    _ ->
      skip_block(LI, Level)
  end.
