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
%%% es_ctype.erl
%%%
%%% Character classification for ErlScheme.
%%%
%%% Extensions:
%%% - : and / are delimiters and thus excluded from initial and subsequent
%%% - [ and ] are delimiters

-module(es_ctype).

-export([ char_is_delimiter/1
        , char_is_initial/1
        , char_is_numeric/1
        , char_is_subsequent/1
        , char_is_whitespace/1
        , char_value/1
        ]).

%% API -------------------------------------------------------------------------

-spec char_is_delimiter(-1 | char()) -> boolean().
char_is_delimiter(Ch) ->
  if Ch < 128 -> char_is_type(Ch, 16#02);
     true -> false
  end.

-spec char_is_initial(-1 | char()) -> boolean().
char_is_initial(Ch) ->
  if Ch < 128 -> char_is_type(Ch, 16#04);
     true -> es_uc_ctype:is_initial(Ch)
  end.

-spec char_is_numeric(-1 | char()) -> boolean().
char_is_numeric(Ch) ->
  if Ch < 128 -> char_is_type(Ch, 16#08);
     true -> false
  end.

-spec char_is_subsequent(-1 | char()) -> boolean().
char_is_subsequent(Ch) ->
  if Ch < 128 -> char_is_type(Ch, 16#10);
     true -> es_uc_ctype:is_subsequent(Ch)
  end.

-spec char_is_whitespace(-1 | char()) -> boolean().
char_is_whitespace(Ch) ->
  if Ch < 128 -> char_is_type(Ch, 16#01);
     true -> es_uc_ctype:is_whitespace(Ch)
  end.

-spec char_value(-1 | char()) -> 0..15 | 255.
char_value(Ch) when Ch > 127 -> 255;
char_value(Ch) ->
  ChValueTab = % indexed by [-1, 255] + 1
    <<
     % EOF                                                             (-1)
     "\xFF"
     % NUL SOH STX ETX EOT ENQ ACK BEL  BS  HT  LF  VT  FF  CR  SO  SI (0-15)
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     % DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN  EM SUB ESC  FS  GS  RS  UA (16-31)
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     % SPC   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / (32-47)
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     %   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? (48-63)
     "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xFF\xFF\xFF\xFF\xFF\xFF"
     %   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O (64-79)
     "\xFF\x0A\x0B\x0C\x0D\x0E\x0F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     %   P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ (80-95)
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     %   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o (96-111)
     "\xFF\x0A\x0B\x0C\x0D\x0E\x0F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     %   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ DEL (112-127)
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF">>,
  binary:at(ChValueTab, Ch + 1).

%% Internals -------------------------------------------------------------------

%% Character classification flag bits:
%%
%% 16#01: whitespace
%% 16#02: delimiter
%% 16#04: initial
%% 16#08: numeric
%% 16#10: subsequent

char_is_type(Ch, Mask) ->
  ChTypeTab = % indexed by [-1, 255] + 1
    <<
     % EOF                                                             (-1)
     "\x02" % EOF is a delimiter but not whitespace
     % NUL SOH STX ETX EOT ENQ ACK BEL  BS  HT  LF  VT  FF  CR  SO  SI (0-15)
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x03\x00\x03\x03\x00\x00"
     % DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN  EM SUB ESC  FS  GS  RS  UA (16-31)
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     % SPC   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   / (32-47)
     "\x03\x14\x02\x00\x14\x14\x14\x00\x02\x02\x14\x10\x00\x10\x10\x02"
     %   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? (48-63)
     "\x18\x18\x18\x18\x18\x18\x18\x18\x18\x18\x02\x02\x14\x14\x14\x14"
     %   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O (64-79)
     "\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14"
     %   P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ (80-95)
     "\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x02\x00\x02\x14\x14"
     %   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o (96-111)
     "\x00\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14"
     %   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~ DEL (112-127)
     "\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x14\x00\x02\x00\x14\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00">>,
  (binary:at(ChTypeTab, Ch + 1) band Mask) =/= 0.
