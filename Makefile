ADA_SRC = src
ADA_TEST = test
OBJ_DIR = obj
BIN_DIR = bin

GNATMAKE = gnatmake
GNATFLAGS = -D $(OBJ_DIR) -aI$(ADA_SRC) -aI$(ADA_TEST)

ADA_SOURCES = $(wildcard $(ADA_SRC)/*.ads $(ADA_SRC)/*.adb)

.PHONY: all clean test

all: $(BIN_DIR)/test_lexer $(BIN_DIR)/test_ls $(BIN_DIR)/test_parser $(BIN_DIR)/test_all $(BIN_DIR)/test_parse_optimade_filter

$(BIN_DIR)/test_lexer: $(ADA_SOURCES) $(ADA_TEST)/test_lexer.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_lexer.adb

$(BIN_DIR)/test_ls: $(ADA_SOURCES) $(ADA_TEST)/test_ls.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_ls.adb

$(BIN_DIR)/test_parser: $(ADA_SOURCES) $(ADA_TEST)/test_parser.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_parser.adb

$(BIN_DIR)/test_all: $(ADA_SOURCES) $(ADA_TEST)/test_all.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_all.adb

$(BIN_DIR)/test_parse_optimade_filter: $(ADA_SOURCES) $(ADA_TEST)/test_parse_optimade_filter.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_parse_optimade_filter.adb

test: $(BIN_DIR)/test_lexer $(BIN_DIR)/test_ls $(BIN_DIR)/test_parser
	@echo "=== Running lexer tests ==="
	$(BIN_DIR)/test_lexer
	@echo ""
	@echo "=== Running language spec tests ==="
	$(BIN_DIR)/test_ls
	@echo ""
	@echo "=== Running parser tests ==="
	$(BIN_DIR)/test_parser

clean:
	rm -f $(OBJ_DIR)/* $(BIN_DIR)/*
