## Process this file with automake to produce Makefile.in

.phony: haddock depgraphs libdoc cabal-modules

# TARGETS are the files that corresponds to executables.
# LIB     are the files that do not correspond to an executable.

SUBDIRS = testsuite/tests

cgibindir = /usr/lib/cgi-bin

TARGETS_SRC   = $(TARGETS:=.hs)
LIB_SRC       = $(wildcard src/[A-Z]*.hs)
ALL_SRC       = $(LIB_SRC) $(TARGETS_SRC)

TESTS = testsuite/tests/testsuit

HADDOCK_DIR = haddock

libdoc:
	$(MKDIR_P) $(HADDOCK_DIR)
	find src -name '*.hs' | xargs $(HADDOCK) --optghc=-isrc -o $(HADDOCK_DIR) --html

depgraphs:
	~/pgm/lng/hs/module-dependency-graph/mod-dep --leafs ObjectModel      ObjectModel      cashflow-hs.cgi.hs > ObjectModel.dot
	~/pgm/lng/hs/module-dependency-graph/mod-dep --leafs ObjectModelTools ObjectModelTools cashflow-hs.cgi.hs > ObjectModelTools.dot

TAGS:
	find src cashflow testsuite -name '*.hs' | xargs hasktags --etags

clean-local:
	$(RM) -f TAGS $(TARGETS) *.hi *.o *~
	$(RM) -rf $(HADDOCK_DIR)

cabal-modules:
	find src -name '*.hs' | sed 's:^src/:    :;s:/:.:g;s:.hs$::'
