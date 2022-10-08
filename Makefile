#   Copyright 2014-2022 Mikael Pettersson
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# Makefile for ErlScheme

SHELL := $(shell command -v bash)
REBAR3 := $(shell type -p rebar3 || echo ./rebar3)
REBAR3_GIT = https://github.com/erlang/rebar3.git
REBAR3_VSN = 3.19.0

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
datarootdir=$(prefix)/share
datadir=$(datarootdir)
docdir=$(datarootdir)/doc/erlscheme-$(VSN)
srcdir=.

EBIN_DIR = _build/default/lib/erlscheme/ebin
SCM_DIR = scm
BIN_DIR = bin

all:	compile $(BIN_DIR)/erlscheme

compile: $(REBAR3) src/es_uc_ctype.erl
	$(REBAR3) do compile, xref, dialyzer, eunit

$(BIN_DIR)/erlscheme:
	mkdir -p $(BIN_DIR)
	sed "s,@EBIN_DIR@,$(EBIN_DIR),g" < make/erlscheme.in > $(BIN_DIR)/erlscheme
	chmod +x $(BIN_DIR)/erlscheme

install:	compile $(BIN_DIR)/erlscheme
	: install .beam files for compiled .erl or .scm code
	mkdir -p $(DESTDIR)$(datadir)/erlscheme/ebin
	cp $(EBIN_DIR)/*.beam $(DESTDIR)$(datadir)/erlscheme/ebin
	: install .scm files
	mkdir -p $(DESTDIR)$(datadir)/erlscheme/scm
	cp $(SCM_DIR)/*.scm $(DESTDIR)$(datadir)/erlscheme/scm
	: install the 'erlscheme' executable
	mkdir -p $(DESTDIR)$(bindir)
	sed "s,@EBIN_DIR@,$(datadir)/erlscheme/ebin,g" < make/erlscheme.in > $(DESTDIR)$(bindir)/erlscheme
	chmod +x $(DESTDIR)$(bindir)/erlscheme

clean distclean realclean:
	rm -rf $(BIN_DIR) _build

# generate src/es_uc_ctype.erl from UnicodeData.txt:
# make UCD=/path/to/otp_src/lib/stdlib/uc_spec/UnicodeData.txt src/es_uc_ctype.erl
src/es_uc_ctype.erl:
	@if [ -z "$(UCD)" ]; then echo UCD not set; exit 1; fi
	make/gen_es_uc_ctype.escript "$(UCD)" src/es_uc_ctype.erl

./rebar3:
	mkdir -p _build; \
	cd _build; \
	git clone --quiet $(REBAR3_GIT); \
	cd rebar3; \
	git checkout --quiet $(REBAR3_VSN); \
	./bootstrap; \
	mv rebar3 ../../; \
	cd ../..; \
	rm -rf _build/rebar3
