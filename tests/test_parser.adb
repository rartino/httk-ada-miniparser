--  Test the parser by parsing the EBNF grammar text and comparing
--  the result with the expected hardcoded AST (mirrors test_parser.py).
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Miniparser.Lexer;         use Miniparser.Lexer;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;
with Miniparser.Parser;        use Miniparser.Parser;

procedure Test_Parser is

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   LS : Language_Spec_Record;

   EBNF_Grammar_Text : constant String :=
     "Optional = ""["" , Rhs , ""]"" ;" & ASCII.LF &
     "Repetition = ""{"" , Rhs , ""}"" ;" & ASCII.LF &
     "Grouping = ""("" , Rhs , "")"" ;" & ASCII.LF &
     "Alteration = Rhs , ""|"" , Rhs ;" & ASCII.LF &
     "Concatenation = Rhs , "","" , Rhs ;" & ASCII.LF &
     ASCII.LF &
     "Rhs = identifier" & ASCII.LF &
     "     | terminal" & ASCII.LF &
     "     | special" & ASCII.LF &
     "     | Optional" & ASCII.LF &
     "     | Repetition" & ASCII.LF &
     "     | Grouping" & ASCII.LF &
     "     | Alteration" & ASCII.LF &
     "     | Concatenation ;" & ASCII.LF &
     ASCII.LF &
     "Rule = identifier , ""="" , Rhs , "";"" ;" & ASCII.LF &
     "Grammar = { Rule } ;" & ASCII.LF;

   --  Expected AST (same structure as Python's ebnf_grammar_ast)
   Expected_AST : AST_Node_Access;

   Result : AST_Node_Access;

begin
   --  Build the LS with hardcoded EBNF AST (same as test_ls.adb)
   LS.EBNF_AST := N ("Grammar",
     N ("Rule",
       N ("identifier", N ("Optional")),
       N ("Concatenation",
         N ("terminal", N ("""[""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""]""")))),
     N ("Rule",
       N ("identifier", N ("Repetition")),
       N ("Concatenation",
         N ("terminal", N ("""{""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""}""")))),
     N ("Rule",
       N ("identifier", N ("Grouping")),
       N ("Concatenation",
         N ("terminal", N ("""(""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""")""")))),
     N ("Rule",
       N ("identifier", N ("Alteration")),
       N ("Concatenation",
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""|""")),
         N ("identifier", N ("Rhs")))),
     N ("Rule",
       N ("identifier", N ("Concatenation")),
       N ("Concatenation",
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""",""")),
         N ("identifier", N ("Rhs")))),
     N ("Rule",
       N ("identifier", N ("Rhs")),
       N ("Alteration",
         N ("identifier", N ("identifier")),
         N ("identifier", N ("terminal")),
         N ("identifier", N ("special")),
         N ("identifier", N ("Optional")),
         N ("identifier", N ("Repetition")),
         N ("identifier", N ("Grouping")),
         N ("identifier", N ("Alteration")),
         N ("identifier", N ("Concatenation")))),
     N ("Rule",
       N ("identifier", N ("Rule")),
       N ("Concatenation",
         N ("identifier", N ("identifier")),
         N ("terminal", N ("""=""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""";""")))),
     N ("Rule",
       N ("identifier", N ("Grammar")),
       N ("Repetition", N ("identifier", N ("Rule")))));

   LS.Start := U ("Grammar");

   LS.Ignore.Insert (" ");
   LS.Ignore.Insert ((1 => ASCII.HT));
   LS.Ignore.Insert ((1 => ASCII.LF));

   LS.Comment_Markers.Append
     ((Start_Mark => U ("(*"), End_Mark => U ("*)")));

   LS.Literals.Insert ("[");
   LS.Literals.Insert ("]");
   LS.Literals.Insert ("{");
   LS.Literals.Insert ("}");
   LS.Literals.Insert ("(");
   LS.Literals.Insert (")");
   LS.Literals.Insert ("|");
   LS.Literals.Insert (",");
   LS.Literals.Insert (";");
   LS.Literals.Insert ("=");

   declare
      P1 : Precedence_Entry;
      P2 : Precedence_Entry;
   begin
      P1.Assoc := U ("left");
      P1.Symbols.Append (U ("|"));
      P2.Assoc := U ("left");
      P2.Symbols.Append (U (","));
      LS.Precedence.Append (P1);
      LS.Precedence.Append (P2);
   end;

   LS.Tokens.Append
     ((Name => U ("identifier"), Pattern => U ("[a-zA-Z][a-zA-Z0-9_]*")));
   LS.Tokens.Append
     ((Name    => U ("terminal"),
       Pattern => U ("""([^\\""]|.)*""|'([^\\']|.)*'")));
   LS.Tokens.Append
     ((Name => U ("special"), Pattern => U ("\?[^?]*\?")));

   LS.Simplify.Append (U ("Rhs"));
   LS.Aggregate.Append (U ("Grammar"));
   LS.Aggregate.Append (U ("Alteration"));
   LS.Aggregate.Append (U ("Concatenation"));
   LS.Remove.Append (U ("["));
   LS.Remove.Append (U ("]"));
   LS.Remove.Append (U ("{"));
   LS.Remove.Append (U ("}"));
   LS.Remove.Append (U ("("));
   LS.Remove.Append (U (")"));
   LS.Remove.Append (U ("|"));
   LS.Remove.Append (U (","));
   LS.Remove.Append (U (";"));
   LS.Remove.Append (U ("="));

   --  Expected AST is the same as the hardcoded EBNF_AST
   Expected_AST := LS.EBNF_AST;

   --  Parse the EBNF grammar text
   Put_Line ("Parsing EBNF grammar text...");
   Result := Parse (LS, EBNF_Grammar_Text);
   Put_Line ("Parse complete.");

   --  Print the result
   Put_Line ("=== Result AST ===");
   Print_AST (Result);

   --  Compare
   Put_Line ("=== Comparing with expected AST ===");
   if AST_Equal (Result, Expected_AST) then
      Put_Line ("PASS: Result matches expected AST.");
   else
      Put_Line ("FAIL: Result does NOT match expected AST.");
      Put_Line ("=== Expected AST ===");
      Print_AST (Expected_AST);
   end if;

end Test_Parser;
