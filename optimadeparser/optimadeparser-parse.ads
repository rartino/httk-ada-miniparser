with Miniparser.Language_Spec; use Miniparser.Language_Spec;

--  OPTIMADE filter string parser and OJF converter.
--
--  Typical usage:
--    Result := Optimadeparser.Parse.Parse ("nelements >= 3");
--    Optimadeparser.Parse.Print_OJF (Result);
--
--  OJF (OPTIMADE JSON Format) Output
--  ----------------------------------
--  The OJF output uses AST_Node trees where Tag holds the operator or value
--  and Children hold the operands.  The following node types are produced:
--
--  Comparisons: (operator, left, right)
--    operator is "=", "!=", "<", "<=", ">", or ">="
--    Example: nelements=3  =>  Tag="=", [Tag="nelements", Tag="3"]
--
--  String operations: (op, property, value)
--    op is "CONTAINS", "STARTS", or "ENDS"
--    Example: name CONTAINS "Al" => Tag="CONTAINS", [Tag="name", ...]
--
--  Known/Unknown: (IS_KNOWN|IS_UNKNOWN, property)
--    Example: nsites IS KNOWN => Tag="IS_KNOWN", [Identifier node]
--
--  Set operations: (HAS|HAS_ALL|HAS_ONLY, operators, property, values)
--    Example: elements HAS "Si"
--      => Tag="HAS_ALL", [ops, property, values]
--
--  Zip operations: (HAS_ZIP, operators, properties, values)
--    Example: elements:elements_ratios HAS "Si":"0.5"
--      => Tag="HAS_ZIP", [ops, properties, values]
--
--  Length: (LENGTH, property, operator, value)
--    Example: elements LENGTH 3
--      => Tag="LENGTH", [property, Tag="=", Tag="3"]
--
--  Logic: (AND|OR, left, right) and (NOT, expr)
--    Example: a=1 AND b=2  => Tag="AND", [left_expr, right_expr]
--    Example: NOT a=1       => Tag="NOT", [expr]
package Optimadeparser.Parse is

   --  Initialize the OPTIMADE parser: reads the EBNF grammar file from
   --  "grammars/optimade_filter_grammar.ebnf" (relative to CWD), configures
   --  OPTIMADE-specific tokens, literals, and skip rules, and builds the
   --  LR(1) parse tables.
   --
   --  Called automatically by Parse and Parse_Raw if not yet done.  Call
   --  explicitly to control when the (relatively expensive) table-building
   --  step occurs, or to set a non-zero Verbosity for diagnostics.
   procedure Initialize (Verbosity : Natural := 0);

   --  Parse an OPTIMADE filter string and return the raw parse tree (an
   --  AST whose structure directly reflects the grammar productions).
   --  Calls Initialize automatically if needed.
   function Parse_Raw
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access;

   --  Parse an OPTIMADE filter string and return the OJF tree (the
   --  simplified intermediate format).  Equivalent to calling Parse_Raw
   --  followed by Parse_Tree_To_OJF.
   function Parse
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access;

   --  Convert a raw parse tree (as returned by Parse_Raw) to OJF
   --  representation.  The root node must have Tag = "Filter".
   --  Raises Program_Error if the tree structure is unexpected.
   function Parse_Tree_To_OJF
     (AST : AST_Node_Access) return AST_Node_Access;

   --  Print an OJF tree to stdout in a format compatible with Python's
   --  pprint output.  Tagged tuples print as ('tag', child1, child2, ...),
   --  unnamed tuples as (child1, child2, ...), and leaves as 'value'.
   --  Long lines are wrapped with indentation to stay within 80 columns.
   procedure Print_OJF (Node : AST_Node_Access);

end Optimadeparser.Parse;
