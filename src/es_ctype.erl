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
%%% es_ctype.erl
%%% Character classification and manipulation for ErlScheme.
%%% This currently assumes that the range of characters is [0..255],
%%% that we represent EOF as -1, and that 7-bit ASCII is contained
%%% in the lower 7 bits of character codes.

-module(es_ctype).

-export([char_is_whitespace/1,	% [-1, 255] -> true | false
	 char_is_delimiter/1,	% [-1, 255] -> true | false
	 char_is_initial/1,	% [-1, 255] -> true | false
	 char_is_numeric/1,	% [-1, 255] -> true | false
	 char_is_subsequent/1,	% [-1, 255] -> true | false
	 char_is_exponent/1,	% [-1, 255] -> true | false
	 char_is_lower_case/1,	% [-1, 255] -> true | false
	 char_is_upper_case/1,	% [-1, 255] -> true | false
	 char_is_alphabetic/1,	% [-1, 255] -> true | false
	 char_is_eof/1,		% [-1, 255] -> true | false
	 char_upcase/1,		% [-1, 255] -> [-1, 255]
	 char_downcase/1,	% [-1, 255] -> [-1, 255]
	 char_value/1]).	% [-1, 255] -> [0-15, 255]

%% Character classification flag bits:
%%
%% 16#01: whitespace
%% 16#02: delimiter	(used by the reader; also allows #\[ and #\])
%% 16#04: initial	(used by the reader; also allows #\@)
%% 16#08: numeric
%% 16#10: subsequent	(used by the reader)
%% 16#20: exponent	(used by the reader)
%% 16#40: lower-case
%% 16#80: upper-case

char_is_type(Ch, Mask) ->
  ChTypeTab = % indexed by [-1, 255] + 1
    <<
     "\x02" % EOF is a delimiter but not whitespace
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x03\x00\x03\x03\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x03\x14\x02\x00\x14\x14\x14\x00\x02\x02\x14\x10\x00\x10\x10\x14"
     "\x18\x18\x18\x18\x18\x18\x18\x18\x18\x18\x14\x02\x14\x14\x14\x14"
     "\x14\x54\x54\x54\x74\x74\x74\x54\x54\x54\x54\x54\x74\x54\x54\x54"
     "\x54\x54\x54\x74\x54\x54\x54\x54\x54\x54\x54\x02\x00\x02\x14\x14"
     "\x00\x94\x94\x94\xB4\xB4\xB4\x94\x94\x94\x94\x94\xB4\x94\x94\x94"
     "\x94\x94\x94\xB4\x94\x94\x94\x94\x94\x94\x94\x00\x00\x00\x14\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
     "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00">>,
  (binary:at(ChTypeTab, Ch + 1) band Mask) =/= 0.

char_is_whitespace(Ch) -> char_is_type(Ch, 16#01).
char_is_delimiter(Ch) -> char_is_type(Ch, 16#02).
char_is_initial(Ch) -> char_is_type(Ch, 16#04).
char_is_numeric(Ch) -> char_is_type(Ch, 16#08).
char_is_subsequent(Ch) -> char_is_type(Ch, 16#10).
char_is_exponent(Ch) -> char_is_type(Ch, 16#20).
char_is_lower_case(Ch) -> char_is_type(Ch, 16#40).
char_is_upper_case(Ch) -> char_is_type(Ch, 16#80).
char_is_alphabetic(Ch) -> char_is_type(Ch, 16#C0).
char_is_eof(Ch) -> Ch =:= -1.

char_upcase(Ch) ->
  ChUpCaseTab = % indexed by [0, 255]
    <<
     "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"
     "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F"
     " !\"#$%&'()*+,-./"
     "0123456789:;<=>?"
     "@ABCDEFGHIJKLMNO"
     "PQRSTUVWXYZ[\\]^_"
     "`ABCDEFGHIJKLMNO"
     "PQRSTUVWXYZ{|}~\7F"
     "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F"
     "\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F"
     "\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF"
     "\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF"
     "\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xCB\xCC\xCD\xCE\xCF"
     "\xD0\xD1\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xDB\xDC\xDD\xDE\xDF"
     "\xE0\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF"
     "\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF">>,
  if Ch =:= -1 -> Ch;
     true -> binary:at(ChUpCaseTab, Ch)
  end.

char_downcase(Ch) ->
  ChDownCaseTab = % indexed by [0, 255]
    <<
     "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"
     "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F"
     " !\"#$%&'()*+,-./"
     "0123456789:;<=>?"
     "@abcdefghijklmno"
     "pqrstuvwxyz[\\]^_"
     "`abcdefghijklmno"
     "pqrstuvwxyz{|}~\7F"
     "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F"
     "\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F"
     "\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF"
     "\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF"
     "\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xCB\xCC\xCD\xCE\xCF"
     "\xD0\xD1\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xDB\xDC\xDD\xDE\xDF"
     "\xE0\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF"
     "\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF">>,
  if Ch =:= -1 -> Ch;
     true -> binary:at(ChDownCaseTab, Ch)
  end.

char_value(Ch) ->
  ChValueTab = % indexed by [-1, 255] + 1
    <<
     "\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\x0A\x0B\x0C\x0D\x0E\x0F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
     "\xFF\x0A\x0B\x0C\x0D\x0E\x0F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
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
