# *httk* miniparser and OPTIMADE filter string parser - Ada Implementation

An Ada reimplementation of the  [*httk*](https://httk.org/) LR(1) miniparser.
The repository includes an implementation to use the LR(1) parser to parse [OPTIMADE](https://www.optimade.org/) filter expressions.
The parsing is made into abstract syntax trees, which are then convert them to a simplified intermediate JSON format "OJF"
(OPTIMADE JSON Format) more suited for interpretation by downstream code than the raw abstract parse trees.

## Quickstart

Build all binaries:
```
make
```

Run the tests:
```
make tests
```

## Architecture

### miniparser: Generic LR(1) Parser Framework

A self-contained LR(1) parser generator. Takes EBNF grammars as text input and
produces parse tables. The only external dependency is the GNAT standard
library (uses `GNAT.Regpat` for regex).

Package hierarchy:

- `Miniparser` — Root package (pure).
- `Miniparser.Lexer` — Tokenizer: splits source strings into token sequences
  using regex patterns, literal matching, and partial token support for greedy
  matching.
- `Miniparser.Language_Spec` — Language specification builder: takes an EBNF
  grammar (as text or a pre-built AST) and produces BNF rules, FIRST sets, and
  LR(1) ACTION/GOTO parse tables.
- `Miniparser.Parser` — LR(1) shift/reduce parser: consumes tokens and produces
  ASTs, with node filtering (simplify, aggregate, remove) on reduce.

### optimadeparser: Parsing of OPTIMADE filter strings

- `Optimadeparser` — Root package (pure).
- `Optimadeparser.Parse` — Defines OPTIMADE tokens, loads the EBNF grammar
  file, initializes the parser via miniparser, and converts raw parse trees to
  OJF format.

## How the Parser Works

This is roughly how the parser operates:

1. **EBNF Parsing.** The parser takes an EBNF grammar as text input. On first
   use, it parses this text into an AST representation of the grammar. To do
   this, it uses a hardcoded AST of the EBNF language itself (the bootstrap
   grammar), so the parser bootstraps itself.

2. **EBNF-to-BNF Conversion.** The EBNF AST is translated to a BNF-like form
   that expands alternation, optionals, groupings, and repetitions into separate
   rules.

3. **Rule Table.** The BNF grammar is processed into a rule table: a map from
   every non-terminal symbol to a list of possible right-hand-side alternatives
   in the production rules.

4. **FIRST Sets.** The rule table is used to build the FIRST(symbol) function
   via fixed-point iteration. It maps each symbol to the set of terminals that
   may appear as the very first token when matching that symbol's production
   rules.

5. **ACTION/GOTO Tables.** The rule table and FIRST sets are used to build the
   LR(1) ACTION and GOTO tables. These encode a state machine that, for every
   parser state, tells the machine whether to shift or reduce, and which state
   to transition to.

Once the parse tables are built, parsing proceeds as:

- The input string is tokenized by the lexer into a sequence of tokens.
- The LR(1) state machine is initialized in its start state. Tokens are consumed
  and shift/reduce actions are made according to the ACTION and GOTO tables.
- When all input has been reduced into the start symbol, the AST connected to
  that symbol is returned.

Parse tables are generated on first use and cached in the `Language_Spec_Record`.

## Output Format

The OPTIMADE parser outputs a JSON-like format "OJF" (OPTIMADE JSON Format), using `AST_Node` trees to represent simplified filter expressions.
The `Tag` field holds the operator or value, and `Children` hold the operands. Leaf nodes have no children.

### Node Types

**Comparisons** — `(operator, left, right)` where operator is `=`, `!=`, `<`,
`<=`, `>`, or `>=`:
```
Tag=">=", Children=[Tag="nelements", Tag="3"]
```

**String operations** — `(op, property, value)` where op is `CONTAINS`,
`STARTS`, or `ENDS`:
```
Tag="CONTAINS", Children=[Tag="chemical_formula", Tag="Si"]
```

**Known/Unknown tests** — `(IS_KNOWN, property)` or `(IS_UNKNOWN, property)`:
```
Tag="IS_KNOWN", Children=[Identifier node]
```

**Set operations** — `(HAS|HAS_ALL|HAS_ONLY, operators, property, values)`:
```
Tag="HAS_ALL", Children=[ops_node, property_node, values_node]
```

**Zip operations** — `(HAS_ZIP, operators, properties, values)`:
```
Tag="HAS_ZIP", Children=[ops_node, properties_node, values_node]
```

**Length comparisons** — `(LENGTH, property, operator, value)`:
```
Tag="LENGTH", Children=[property_node, op_node, value_node]
```

**Logical operators** — `(AND, left, right)`, `(OR, left, right)`,
`(NOT, expr)`:
```
Tag="AND", Children=[left_expr, right_expr]
Tag="NOT", Children=[expr]
```

### Using the Parser in Ada Code

Basic usage with the OPTIMADE parser:

```ada
with Optimadeparser.Parse;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;

procedure Example is
   Result : AST_Node_Access;
begin
   --  Parse a filter string into OJF format.
   --  Initialize is called automatically on first use.
   Result := Optimadeparser.Parse.Parse ("nelements >= 3");
   Optimadeparser.Parse.Print_OJF (Result);
end Example;
```

Using miniparser directly with a custom grammar:

```ada
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Miniparser.Lexer;          use Miniparser.Lexer;
with Miniparser.Language_Spec;  use Miniparser.Language_Spec;
with Miniparser.Parser;

procedure Custom_Grammar is
   LS     : Language_Spec_Record;
   Result : AST_Node_Access;
begin
   --  Set up language specification
   LS.EBNF_Grammar := To_Unbounded_String
     ("S = E ;" &
      "E = T , '+' , E ;" &
      "E = T ;" &
      "T = id ;");
   LS.Start := To_Unbounded_String ("S");
   LS.Ignore.Insert (" ");
   LS.Literals.Insert ("+");
   LS.Tokens.Append
     ((Name    => To_Unbounded_String ("id"),
       Pattern => To_Unbounded_String ("[a-zA-Z][a-zA-Z0-9_]*")));
   LS.Remove.Append (To_Unbounded_String ("+"));

   --  Parse tables are built automatically on first Parse call
   Result := Miniparser.Parser.Parse (LS, "Test + Test");
   Print_AST (Result);
end Custom_Grammar;
```

### Diagnostic Output

Pass `Verbosity => N` to `Parse`, `Build_LS`, or `Initialize` to get
diagnostic output on stdout:

- Verbosity 0: silent (default).
- Verbosity 3+: prints parser state transitions and shift/reduce actions.
- Verbosity 4+: prints symbol stack summary during parsing, lexer token output.
- Verbosity 5+: prints detailed symbol stack and lexer character input.

## License

This package is licensed under the MIT License, provided in the file [LICENSE](LICENSE).
