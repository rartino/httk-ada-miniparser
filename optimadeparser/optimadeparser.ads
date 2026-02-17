--  Optimadeparser: OPTIMADE filter string parser — Ada implementation
--
--  OPTIMADE (Open Databases Integration for Materials Design) is an API
--  specification for querying materials science databases.  Filter strings
--  are the query language, e.g. "nelements >= 3 AND chemical_formula_hill
--  CONTAINS "Si"".
--
--  This package parses OPTIMADE filter expressions and converts them to
--  OJF (OPTIMADE JSON Format) representation — a simplified intermediate
--  format using tagged AST_Node trees that mirror the nested-tuple format
--  of the Python implementation.
--
--  The child package Optimadeparser.Parse provides the public API:
--    Initialize      — Pre-build parse tables (optional, called lazily).
--    Parse_Raw       — Parse to raw parse tree.
--    Parse           — Parse to OJF tree.
--    Parse_Tree_To_OJF — Convert a raw tree to OJF.
--    Print_OJF       — Pretty-print OJF in Python pprint-compatible format.

package Optimadeparser is
   pragma Pure;
end Optimadeparser;
