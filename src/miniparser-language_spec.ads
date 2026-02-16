with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Miniparser.Lexer;

package Miniparser.Language_Spec is

   use Miniparser.Lexer;

   --  AST Node for EBNF grammar trees
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

   --  Language Specification
   type Language_Spec_Record is record
      --  Configuration
      Start          : Unbounded_String := Null_Unbounded_String;
      Ignore         : String_Sets.Set;
      Comment_Markers : Comment_Vectors.Vector;
      Tokens         : Token_Def_Vectors.Vector;
      Partial_Tokens : Token_Def_Vectors.Vector;
      Literals       : String_Sets.Set;
      Precedence     : Prec_Vectors.Vector;
      Simplify       : UString_Vectors.Vector;
      Aggregate      : UString_Vectors.Vector;
      Remove         : UString_Vectors.Vector;
      Skip_Rules     : UString_Vectors.Vector;

      --  Grammar source (EBNF text, for bootstrap parsing)
      EBNF_Grammar   : Unbounded_String := Null_Unbounded_String;

      --  AST
      EBNF_AST       : AST_Node_Access := null;
      BNF_Grammar    : BNF_Vectors.Vector;

      --  Computed
      Terminals      : String_Sets.Set;
      Rule_Table     : Rule_Maps.Map;
      First_Table    : First_Maps.Map;
      Action_Table   : Action_Maps.Map;
      Goto_Table     : Goto_Maps.Map;

      --  Flags
      Has_BNF        : Boolean := False;
      Has_Terminals  : Boolean := False;
      Has_Rules      : Boolean := False;
      Has_First      : Boolean := False;
      Has_Parse      : Boolean := False;
   end record;

   Parser_Grammar_Error : exception;

   procedure Build_LS
     (LS        : in out Language_Spec_Record;
      Verbosity : Natural := 0);

   --  AST constructors
   function N (Tag : String) return AST_Node_Access;
   function N (Tag : String; C1 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2, C3 : AST_Node_Access) return AST_Node_Access;
   function N (Tag : String; C1, C2, C3, C4 : AST_Node_Access)
     return AST_Node_Access;
   function N (Tag : String; C1, C2, C3, C4, C5, C6, C7, C8 : AST_Node_Access)
     return AST_Node_Access;

   --  AST comparison (recursive tree equality)
   function AST_Equal (A, B : AST_Node_Access) return Boolean;

   --  AST debug printing
   procedure Print_AST (Node : AST_Node_Access; Indent : Natural := 0);

   --  Printing
   procedure Print_LS (LS : Language_Spec_Record);

end Miniparser.Language_Spec;
