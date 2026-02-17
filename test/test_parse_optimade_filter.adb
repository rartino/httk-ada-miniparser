--  Test program for OPTIMADE filter parser OJF conversion.
--  Mirrors src/test_parse_optimade_filter.py
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;
with Optimadeparser.Parse;  use Optimadeparser.Parse;

procedure Test_Parse_Optimade_Filter is
   Input_String : Unbounded_String;
   OJF : AST_Node_Access;
begin
   if Ada.Command_Line.Argument_Count >= 1 then
      Input_String := To_Unbounded_String (Ada.Command_Line.Argument (1));
   else
      Input_String := To_Unbounded_String
        ("elements HAS ALL ""Ga"",""Ti"""
         & " AND (nelements=3 OR nelements=2)");
   end if;

   OJF := Parse (To_String (Input_String));

   Put_Line ("==== FILTER STRING PARSE RESULT:");
   Print_OJF (OJF);
   Put_Line ("====");
end Test_Parse_Optimade_Filter;
