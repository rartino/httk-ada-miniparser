--  End-to-end test: reads EBNF grammar from file, builds LS via
--  full bootstrap (EBNF text -> parse via LS_EBNF -> build tables),
--  then parses an OPTIMADE filter string.  Mirrors test_all.py.
--
--  NOTE: Requires optimade_filter_grammar.ebnf to exist in src/.
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Miniparser.Lexer;         use Miniparser.Lexer;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;
with Miniparser.Parser;        use Miniparser.Parser;

procedure Test_All is

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   function S (U : Unbounded_String) return String
     renames To_String;

   --  Read entire file into a string
   function Read_File (Path : String) return String is
      F   : Ada.Text_IO.File_Type;
      Buf : Unbounded_String;
   begin
      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Path);
      while not Ada.Text_IO.End_Of_File (F) loop
         Append (Buf, Ada.Text_IO.Get_Line (F));
         Append (Buf, ASCII.LF);
      end loop;
      Ada.Text_IO.Close (F);
      return S (Buf);
   end Read_File;

   Grammar_Path : constant String := "../src/optimade_filter_grammar.ebnf";

   LS : Language_Spec_Record;
   Result : AST_Node_Access;

   Filter_String : constant String :=
     "elements HAS ALL ""Ga"",""Ti"""
     & " AND (nelements=3 OR nelements=2)";

begin
   --  Read EBNF grammar from file
   Put_Line ("Reading grammar from " & Grammar_Path & "...");
   declare
      Grammar : constant String := Read_File (Grammar_Path);
   begin
      Put_Line ("Grammar length:" & Natural'Image (Grammar'Length));

      --  Set EBNF grammar text (Build_LS will bootstrap-parse it)
      LS.EBNF_Grammar := U (Grammar);
   end;

   LS.Start := U ("Filter");

   --  Ignore whitespace
   LS.Ignore.Insert (" ");
   LS.Ignore.Insert ((1 => ASCII.HT));
   LS.Ignore.Insert ((1 => ASCII.LF));

   --  Keyword literals
   declare
      Keywords : constant array (1 .. 16) of access constant String :=
        (new String'("AND"), new String'("NOT"), new String'("OR"),
         new String'("KNOWN"), new String'("UNKNOWN"), new String'("IS"),
         new String'("CONTAINS"), new String'("STARTS"),
         new String'("ENDS"), new String'("WITH"), new String'("LENGTH"),
         new String'("HAS"), new String'("ALL"), new String'("ONLY"),
         new String'("EXACTLY"), new String'("ANY"));
   begin
      for KW of Keywords loop
         LS.Literals.Insert (KW.all);
      end loop;
   end;
   --  Whitespace literals
   LS.Literals.Insert (" ");
   LS.Literals.Insert ((1 => ASCII.HT));
   LS.Literals.Insert ((1 => ASCII.LF));
   LS.Literals.Insert ((1 => ASCII.CR));

   --  Token definitions (Appendix 3 of OPTIMADE spec)
   LS.Tokens.Append
     ((Name => U ("Operator"), Pattern => U ("<|<=|>|>=|=|!=")));
   LS.Tokens.Append
     ((Name => U ("Identifier"), Pattern => U ("[a-z_][a-z_0-9]*")));
   LS.Tokens.Append
     ((Name    => U ("String"),
       Pattern => U ("""[^""\\]*(\\.[^""\\]*)*""")));
   LS.Tokens.Append
     ((Name    => U ("Number"),
       Pattern =>
         U ("[-+]?([0-9]+(\.[0-9]*)?|\.[0-9]+)"
            & "([eE][-+]?[0-9]+)?")));
   LS.Tokens.Append
     ((Name => U ("OpeningBrace"), Pattern => U ("\(")));
   LS.Tokens.Append
     ((Name => U ("ClosingBrace"), Pattern => U ("\)")));
   LS.Tokens.Append
     ((Name => U ("Dot"), Pattern => U ("\.")));
   LS.Tokens.Append
     ((Name => U ("Colon"), Pattern => U (":")));
   LS.Tokens.Append
     ((Name => U ("Comma"), Pattern => U (",")));

   --  Partial tokens for greedy number matching
   LS.Partial_Tokens.Append
     ((Name    => U ("Number"),
       Pattern => U ("[-+]?[0-9]+\.?[0-9]*[eE]?[-+]?[0-9]*")));

   --  Skip rules (handled by token regexes, not grammar rules)
   LS.Skip_Rules.Append (U ("EscapedChar"));
   LS.Skip_Rules.Append (U ("UnescapedChar"));
   LS.Skip_Rules.Append (U ("Punctuator"));
   LS.Skip_Rules.Append (U ("Exponent"));
   LS.Skip_Rules.Append (U ("Sign"));
   LS.Skip_Rules.Append (U ("Digits"));
   LS.Skip_Rules.Append (U ("Digit"));
   LS.Skip_Rules.Append (U ("Letter"));
   LS.Skip_Rules.Append (U ("Operator"));
   LS.Skip_Rules.Append (U ("UppercaseLetter"));
   LS.Skip_Rules.Append (U ("LowercaseLetter"));
   LS.Skip_Rules.Append (U ("OpeningBrace"));
   LS.Skip_Rules.Append (U ("Dot"));
   LS.Skip_Rules.Append (U ("ClosingBrace"));
   LS.Skip_Rules.Append (U ("Comma"));
   LS.Skip_Rules.Append (U ("Colon"));

   --  Remove braces from parse tree
   LS.Remove.Append (U (")"));
   LS.Remove.Append (U ("("));

   --  Build language specification (triggers full bootstrap)
   Put_Line ("Building language specification...");
   Build_LS (LS);
   Put_Line ("Build complete!");

   --  Parse filter string
   Put_Line ("Parsing: " & Filter_String);
   Result := Parse (LS, Filter_String);

   --  Print result
   Put_Line ("=== Parse Result ===");
   Print_AST (Result);

end Test_All;
