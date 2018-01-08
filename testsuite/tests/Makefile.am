## Process this file with automake to produce Makefile.in

# TARGETS are the files that corresponds to executables.
# LIB     are the files that do not correspond to an executable.

noinst_PROGRAMS = testsuit

TARGETS = $(noinst_PROGRAMS)

TARGETS_SRC = $(TARGETS:=.hs)
TEST_SRC = \
 TestSuit.hs \
 TestResources/TestUtils.hs \
 TestResources/UserInteractionOutputMonadUtils.hs \
 TestResources/TestData.hs \
 TestResources/ReferenceObjectType.hs \
 Wilde/ObjectModelTest.hs \
 Wilde/DdlUtils.hs \
 Wilde/ObjectModelUserInteractionTest.hs \
 Wilde/Media/ElementTest.hs \
 Wilde/CliToolTest.hs \
 Wilde/ApplicationConstruction/ApplicationObjectModelToolsTest.hs \
 Wilde/ApplicationConstruction/StandardServices/WildeSqlInputerTest.hs \
 Wilde/ApplicationConstruction/SqlExprParserTest.hs \
 Wilde/AttributeTypesListFooterTest.hs \
 Wilde/ListUtilsTest.hs \
 Wilde/DateParserTest.hs


TESTAPP_SRC_DIR = ../testapp/src

PRJ_ROOT_DIR = ../..
LIB_SRC_DIR  = $(PRJ_ROOT_DIR)/src

TESTAPP_SRC = $(TESTAPP_SRC_DIR)/TestApplication.hs \
              $(TESTAPP_SRC_DIR)/TestApplicationServices.hs

ALL_SRC = $(TESTAPP_SRC) $(TEST_SRC) $(TARGETS_SRC)

testsuit_SOURCES = testsuit.hs $(ALL_SRC)

SRC_DIRS = $(LIB_SRC_DIR):$(TESTAPP_SRC_DIR)

$(TARGETS): %: %.hs $(ALL_SRC)
	$(GHC) -i$(SRC_DIRS) --make -o $@ $<

clean-local:
	$(RM) -f $(TARGETS) TAGS *.hi *.o *~
	find . \( -name '*.hi' -o -name '*.o' \) -delete

# $(TARGET_MODULES): %: %.hs $(SRC_COMMON)
# 	ghc -i../.. --make $<

TAGS: $(ALL_SRC)
	hasktags --etags $(ALL_SRC)
