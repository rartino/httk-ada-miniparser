with Miniparser.Language_Spec; use Miniparser.Language_Spec;

package Optimadeparser.Parse is

   --  Initialize the OPTIMADE parser (reads grammar, builds tables).
   --  Called automatically by Parse/Parse_Raw if not yet done.
   procedure Initialize (Verbosity : Natural := 0);

   --  Parse filter string, return raw parse tree.
   function Parse_Raw
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access;

   --  Parse filter string, return OJF tree.
   function Parse
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access;

   --  Convert raw parse tree to OJF representation.
   function Parse_Tree_To_OJF
     (AST : AST_Node_Access) return AST_Node_Access;

   --  Print OJF tree in Python pprint-compatible format.
   procedure Print_OJF (Node : AST_Node_Access);

end Optimadeparser.Parse;
