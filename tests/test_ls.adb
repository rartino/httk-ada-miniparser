with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Miniparser.Lexer;         use Miniparser.Lexer;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;

procedure Test_LS is

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   LS : Language_Spec_Record;

begin
   --  Build the EBNF grammar AST (same as Python test_ls.py)
   LS.EBNF_AST := N ("Grammar",
     --  Rule: Optional = "[" , Rhs , "]" ;
     N ("Rule",
       N ("identifier", N ("Optional")),
       N ("Concatenation",
         N ("terminal", N ("""[""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""]""")))),
     --  Rule: Repetition = "{" , Rhs , "}" ;
     N ("Rule",
       N ("identifier", N ("Repetition")),
       N ("Concatenation",
         N ("terminal", N ("""{""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""}""")))),
     --  Rule: Grouping = "(" , Rhs , ")" ;
     N ("Rule",
       N ("identifier", N ("Grouping")),
       N ("Concatenation",
         N ("terminal", N ("""(""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""")""")))),
     --  Rule: Alteration = Rhs , "|" , Rhs ;
     N ("Rule",
       N ("identifier", N ("Alteration")),
       N ("Concatenation",
         N ("identifier", N ("Rhs")),
         N ("terminal", N ("""|""")),
         N ("identifier", N ("Rhs")))),
     --  Rule: Concatenation = Rhs , "," , Rhs ;
     N ("Rule",
       N ("identifier", N ("Concatenation")),
       N ("Concatenation",
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""",""")),
         N ("identifier", N ("Rhs")))),
     --  Rule: Rhs = identifier | terminal | special | ...
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
     --  Rule: Rule = identifier , "=" , Rhs , ";" ;
     N ("Rule",
       N ("identifier", N ("Rule")),
       N ("Concatenation",
         N ("identifier", N ("identifier")),
         N ("terminal", N ("""=""")),
         N ("identifier", N ("Rhs")),
         N ("terminal", N (""";""")))),
     --  Rule: Grammar = { Rule } ;
     N ("Rule",
       N ("identifier", N ("Grammar")),
       N ("Repetition", N ("identifier", N ("Rule")))));

   --  Configuration matching Python test_ls.py
   LS.Start := U ("Grammar");

   --  Ignore: ' \t\n' -> individual characters
   LS.Ignore.Insert (" ");
   LS.Ignore.Insert ((1 => ASCII.HT));
   LS.Ignore.Insert ((1 => ASCII.LF));

   --  Comment markers
   LS.Comment_Markers.Append
     ((Start_Mark => U ("(*"), End_Mark => U ("*)")));

   --  Literals
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

   --  Precedence: (('left', '|'), ('left', ','))
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

   --  Tokens
   LS.Tokens.Append
     ((Name => U ("identifier"), Pattern => U ("[a-zA-Z][a-zA-Z0-9_]*")));
   LS.Tokens.Append
     ((Name    => U ("terminal"),
       Pattern => U ("""([^\\""]|.)*""|'([^\\']|.)*'")));
   LS.Tokens.Append
     ((Name => U ("special"), Pattern => U ("\?[^?]*\?")));

   --  Simplify, Aggregate, Remove
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

   --  Build!
   Put_Line ("Building language specification...");
   Build_LS (LS);
   Put_Line ("Done!");
   New_Line;

   Print_LS (LS);

end Test_LS;
