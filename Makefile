ADA_SRC = src
ADA_TEST = test
OBJ_DIR = obj
BIN_DIR = bin

GNATMAKE = gnatmake
GNATFLAGS = -D $(OBJ_DIR) -aI$(ADA_SRC) -aI$(ADA_TEST)

.PHONY: all clean test

all: $(BIN_DIR)/test_lexer

$(BIN_DIR)/test_lexer: $(ADA_SRC)/*.ads $(ADA_SRC)/*.adb $(ADA_TEST)/test_lexer.adb
	$(GNATMAKE) -o $@ $(GNATFLAGS) $(ADA_TEST)/test_lexer.adb

test: $(BIN_DIR)/test_lexer
	@echo "=== Running lexer tests ==="
	$(BIN_DIR)/test_lexer

clean:
	rm -f $(OBJ_DIR)/* $(BIN_DIR)/*
