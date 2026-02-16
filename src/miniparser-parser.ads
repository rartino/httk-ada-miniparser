with Miniparser.Language_Spec; use Miniparser.Language_Spec;

package Miniparser.Parser is

   --  Parse source string using the given language specification.
   --  If LS does not have parse tables yet, Build_LS is called first.
   --  Returns the root AST node of the parse tree.
   function Parse
     (LS        : in out Language_Spec_Record;
      Source    : String;
      Verbosity : Natural := 0) return AST_Node_Access;

   Parser_Syntax_Error  : exception;
   Parser_Internal_Error : exception;

end Miniparser.Parser;
