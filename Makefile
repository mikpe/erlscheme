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

VSN := $(shell git describe --dirty)

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
datarootdir=$(prefix)/share
datadir=$(datarootdir)
docdir=$(datarootdir)/doc/erlscheme-$(VSN)
srcdir=.

SHELL = /bin/sh
ERLC = erlc
ERLC_FLAGS = +debug_info +warn_obsolete_guard +warn_export_all -DVSN=\"$(VSN)\"
EBIN_DIR = ebin
SRC_DIR = src
SCM_DIR = scm
BIN_DIR = bin

ERL_SOURCES := $(wildcard $(SRC_DIR)/*.erl)
ERL_OBJECTS := $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(ERL_SOURCES))

.SUFFIXES: .erl .beam

all:	$(ERL_OBJECTS) $(BIN_DIR)/erlscheme

$(ERL_OBJECTS): | $(EBIN_DIR)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(BIN_DIR)/erlscheme:
	mkdir -p $(BIN_DIR)
	echo '#!/bin/sh' > $(BIN_DIR)/erlscheme
	echo "exec /usr/bin/env erl -pa $(EBIN_DIR) -noshell -s es_repl start -s erlang halt" >> $(BIN_DIR)/erlscheme
	chmod +x $(BIN_DIR)/erlscheme

install:	$(ERL_OBJECTS) $(BIN_DIR)/erlscheme
	: install .beam files for compiled .erl or .scm code
	mkdir -p $(DESTDIR)$(datadir)/erlscheme/ebin
	cp $(EBIN_DIR)/*.beam $(DESTDIR)$(datadir)/erlscheme/ebin
	: install .scm files
	mkdir -p $(DESTDIR)$(datadir)/erlscheme/scm
	cp $(SCM_DIR)/*.scm $(DESTDIR)$(datadir)/erlscheme/scm
	: install the 'erlscheme' executable
	mkdir -p $(DESTDIR)$(bindir)
	sed s,$(EBIN_DIR),$(datadir)/erlscheme/ebin,g < $(BIN_DIR)/erlscheme > $(DESTDIR)$(bindir)/erlscheme
	chmod +x $(DESTDIR)$(bindir)/erlscheme

clean distclean realclean:
	rm -rf $(EBIN_DIR) $(BIN_DIR)
