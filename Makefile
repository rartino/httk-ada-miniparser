ADA_SRC = src
ADA_TESTS = tests
ADA_OPTPARSE = optimadeparser
OBJ_DIR = obj
BIN_DIR = bin

GNATMAKE = gnatmake
GNATFLAGS = -D $(OBJ_DIR) -aI$(ADA_SRC) -aI$(ADA_TESTS)

ADA_SOURCES = $(wildcard $(ADA_SRC)/*.ads $(ADA_SRC)/*.adb)

.PHONY: testbins clean tests

testbins: $(BIN_DIR)/test_lexer $(BIN_DIR)/test_ls $(BIN_DIR)/test_parser $(BIN_DIR)/test_all $(BIN_DIR)/test_parse_optimade_filter

$(BIN_DIR)/test_lexer: $(ADA_SOURCES) $(ADA_TESTS)/test_lexer.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TESTS)/test_lexer.adb

$(BIN_DIR)/test_ls: $(ADA_SOURCES) $(ADA_TESTS)/test_ls.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TESTS)/test_ls.adb

$(BIN_DIR)/test_parser: $(ADA_SOURCES) $(ADA_TESTS)/test_parser.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TESTS)/test_parser.adb

$(BIN_DIR)/test_all: $(ADA_SOURCES) $(ADA_TESTS)/test_all.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TESTS)/test_all.adb

$(BIN_DIR)/test_parse_optimade_filter: $(ADA_SOURCES) $(ADA_TESTS)/test_parse_optimade_filter.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) -aI$(ADA_OPTPARSE) $(ADA_TESTS)/test_parse_optimade_filter.adb

tests: $(BIN_DIR)/test_lexer $(BIN_DIR)/test_ls $(BIN_DIR)/test_parser
	@echo "=== Running lexer test ==="
	$(BIN_DIR)/test_lexer
	@echo ""
	@echo "=== Running language spec test ==="
	$(BIN_DIR)/test_ls
	@echo ""
	@echo "=== Running parser test ==="
	$(BIN_DIR)/test_parser
	@echo ""
	@echo "=== Running system test ==="
	$(BIN_DIR)/test_all
	@echo ""
	@echo "=== Running optimade parser test ==="
	$(BIN_DIR)/test_parse_optimade_filter
	$(BIN_DIR)/test_parse_optimade_filter 'a:b HAS "Ga":"Po"'

clean:
	rm -f $(OBJ_DIR)/* $(BIN_DIR)/*
