with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Miniparser.Lexer;

package Miniparser.Language_Spec is

   use Miniparser.Lexer;

   --  AST Node — the universal tree type used for both grammar ASTs and
   --  parse-tree results.
   --
   --  A leaf node has an empty Children vector; its Tag holds the token value.
   --  An interior node has Tag = the grammar symbol name and Children holding
   --  the sub-nodes.  For example, parsing "x + y" with a simple grammar
   --  might produce:  (Tag="E", Children=>[(Tag="+", ...), ...]).
   type AST_Node;
   type AST_Node_Access is access all AST_Node;
   package AST_Vectors is new Ada.Containers.Vectors (Positive, AST_Node_Access);
   type AST_Node is record
      Tag      : Unbounded_String := Null_Unbounded_String;
      Children : AST_Vectors.Vector;
   end record;

   --  BNF Rule: (LHS, RHS-symbol-list)
   type BNF_Rule is record
      LHS : Unbounded_String;
      RHS : UString_Vectors.Vector;
   end record;
   package BNF_Vectors is new Ada.Containers.Vectors (Positive, BNF_Rule);

   --  String sets
   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   --  Rule table: symbol -> list of RHS alternatives
   package RHS_List is new Ada.Containers.Vectors
     (Positive, UString_Vectors.Vector, UString_Vectors."=");
   package Rule_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, RHS_List.Vector, "<", RHS_List."=");

   --  First table: symbol -> set of terminal symbols
   package First_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String_Sets.Set, "<", String_Sets."=");

   --  Action table: state -> (symbol -> action)
   type Action_Kind is (Shift, Reduce, Accept_Action);
   type Action_Entry is record
      Kind       : Action_Kind := Shift;
      New_State  : Natural := 0;
      Reduce_LHS : Unbounded_String := Null_Unbounded_String;
      Reduce_RHS : UString_Vectors.Vector;
   end record;
   package Action_Inner is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Action_Entry);
   package Action_Maps is new Ada.Containers.Ordered_Maps
     (Positive, Action_Inner.Map, "<", Action_Inner."=");

   --  Goto table: state -> (symbol -> state)
   package Goto_Inner is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Positive);
   package Goto_Maps is new Ada.Containers.Ordered_Maps
     (Positive, Goto_Inner.Map, "<", Goto_Inner."=");

   --  Precedence
   type Precedence_Entry is record
      Assoc   : Unbounded_String;
      Symbols : UString_Vectors.Vector;
   end record;
   package Prec_Vectors is new Ada.Containers.Vectors (Positive, Precedence_Entry);

   --  Language Specification — holds all configuration and computed tables
   --  needed to lex and parse a language.
   --
   --  Users populate the configuration fields (Start through Skip_Rules) and
   --  either EBNF_Grammar (text) or EBNF_AST (pre-built AST), then call
   --  Build_LS.  Alternatively, the Parser.Parse function calls Build_LS
   --  automatically on first use.
   type Language_Spec_Record is record
      --  Configuration fields (set by the user before calling Build_LS)

      Start          : Unbounded_String := Null_Unbounded_String;
      --  The start (topmost) symbol of the grammar.  A successful parse means
      --  reducing all input into this symbol.  If not set, defaults to the LHS
      --  of the first BNF rule.

      Ignore         : String_Sets.Set;
      --  Set of single-character strings that the lexer matches but does not
      --  emit.  Commonly whitespace: {" ", HT, LF}.

      Comment_Markers : Comment_Vectors.Vector;
      --  List of (start, end) marker pairs for comment stripping.

      Tokens         : Token_Def_Vectors.Vector;
      --  Ordered list of (name, regex_pattern) pairs defining token types.
      --  These are considered terminals in the parsing.  They may also appear
      --  as production rules in the EBNF, but those definitions are ignored.

      Partial_Tokens : Token_Def_Vectors.Vector;
      --  Ordered list of (name, regex_pattern) pairs for partial token
      --  matches.  Used for greedy matching of multi-part tokens (e.g. to
      --  match "5.32e6" as one Number rather than Number+Identifier+Number).

      Literals       : String_Sets.Set;
      --  Set of literal strings (1 or more characters) that define literal
      --  symbols of the language.  The tokenizer names these tokens the same
      --  as the matched string.

      Precedence     : Prec_Vectors.Vector;
      --  List of (associativity, symbols) entries.  Order defines precedence:
      --  later in the list = higher precedence.  Associativity can be "left",
      --  "right", or "noassoc".  Used to resolve shift/reduce conflicts.

      Simplify       : UString_Vectors.Vector;
      --  List of symbol names that are simplified away when the parse tree is
      --  generated: their children are inlined into the parent node.

      Aggregate      : UString_Vectors.Vector;
      --  List of symbol names that, when constituting consecutive nodes of
      --  the same type, are flattened.  Removes the ambiguity of left/right
      --  associativity in the resulting tree.

      Remove         : UString_Vectors.Vector;
      --  List of symbols to skip entirely in the output parse tree.  Useful
      --  for discarding uninteresting literals like parentheses.

      Skip_Rules     : UString_Vectors.Vector;
      --  List of rules to completely ignore in the grammar.  Useful to skip
      --  EBNF rules that decompose tokens into individual characters, when
      --  those tokens are handled by regex via the Tokens parameter instead.

      --  Grammar source (provide one of these)

      EBNF_Grammar   : Unbounded_String := Null_Unbounded_String;
      --  EBNF grammar text.  Parsed into EBNF_AST on first Build_LS call.

      EBNF_AST       : AST_Node_Access := null;
      --  Pre-built AST of the grammar.  If provided, EBNF_Grammar is not
      --  needed (the text-parsing step is skipped).

      --  Computed fields (populated by Build_LS)

      BNF_Grammar    : BNF_Vectors.Vector;
      --  BNF rules produced by EBNF-to-BNF conversion.

      Terminals      : String_Sets.Set;
      --  Set of terminal symbols (token names + literals + epsilon).

      Rule_Table     : Rule_Maps.Map;
      --  Maps each non-terminal to its list of RHS alternatives.

      First_Table    : First_Maps.Map;
      --  FIRST sets: maps each symbol to the set of terminals that may begin
      --  its derivations.

      Action_Table   : Action_Maps.Map;
      --  LR(1) ACTION table: state -> (symbol -> action).

      Goto_Table     : Goto_Maps.Map;
      --  LR(1) GOTO table: state -> (non-terminal -> state).

      --  Flags tracking which build steps have been completed
      Has_BNF        : Boolean := False;
      Has_Terminals  : Boolean := False;
      Has_Rules      : Boolean := False;
      Has_First      : Boolean := False;
      Has_Parse      : Boolean := False;
   end record;

   --  Raised on grammar errors: ambiguous grammars (reduce/reduce conflicts),
   --  unrecognized symbols, or missing grammar input.
   Parser_Grammar_Error : exception;

   --  Build a language specification from an EBNF grammar and language
   --  meta-information.
   --
   --  Populates the computed fields of LS (BNF_Grammar, Terminals,
   --  Rule_Table, First_Table, Action_Table, Goto_Table) by running the
   --  5-step pipeline described in the Miniparser package comment.
   --
   --  Each step is guarded by a Has_* flag and only runs if not already done,
   --  so calling Build_LS multiple times is safe and idempotent.
   --
   --  If LS.EBNF_AST is null and LS.EBNF_Grammar is set, the EBNF text is
   --  parsed using the bootstrap EBNF parser (which is itself built via
   --  Build_LS on a hardcoded EBNF grammar AST).
   --
   --  Raises Parser_Grammar_Error if the grammar is missing or contains
   --  errors (undefined symbols, reduce/reduce conflicts, etc.).
   procedure Build_LS
     (LS        : in out Language_Spec_Record;
      Verbosity : Natural := 0);

   --  AST constructors — convenience functions for building AST trees.
   --  N("tag") creates a leaf; N("tag", C1, C2, ...) creates an interior
   --  node with the given children.  Overloaded for 0 to 8 children.
   function N (Tag : String) return AST_Node_Access;
   function N (Tag : String; C1 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2, C3 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2, C3, C4 : AST_Node_Access)
     return AST_Node_Access;
   function N (Tag : String; C1, C2, C3, C4, C5, C6, C7, C8 : AST_Node_Access)
     return AST_Node_Access;

   --  Recursive structural equality test for two AST trees.
   --  Two nodes are equal if they have the same Tag and the same number of
   --  children, and all corresponding children are recursively equal.
   --  Two null pointers are considered equal; a null and a non-null are not.
   function AST_Equal (A, B : AST_Node_Access) return Boolean;

   --  Print an AST tree to stdout in an indented text format for debugging.
   --  Leaf nodes print as 'tag'; interior nodes print as ('tag' ...).
   procedure Print_AST (Node : AST_Node_Access; Indent : Natural := 0);

   --  Print all computed tables in a Language_Spec_Record to stdout for
   --  debugging: BNF grammar, rule table, FIRST table, terminals, ACTION
   --  table, and GOTO table.
   procedure Print_LS (LS : Language_Spec_Record);

end Miniparser.Language_Spec;
