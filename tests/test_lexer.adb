with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Miniparser.Lexer;      use Miniparser.Lexer;

procedure Test_Lexer is

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   procedure Print_Token (T : Token_Entry) is
   begin
      if T.Is_End then
         Put ("(None, None, (");
      else
         Put ("('" & To_String (T.Name) & "', '"
              & To_String (T.Value) & "', (");
      end if;
      Put (Natural'Image (T.Pos.Line) & ","
           & Natural'Image (T.Pos.Column) & ", '"
           & To_String (T.Pos.Line_Text) & "'))");
      New_Line;
   end Print_Token;

   Tokens         : Token_Def_Vectors.Vector;
   Partial_Tokens : Token_Def_Vectors.Vector;
   Literals       : UString_Vectors.Vector;
   Result         : Token_Entry_Vectors.Vector;

begin
   ---------------------------------------------------------------
   --  Test 1: simple expression "Test + Who - Man"
   ---------------------------------------------------------------
   Tokens.Clear;
   Partial_Tokens.Clear;
   Literals.Clear;

   Tokens.Append ((Name => U ("id"),
                    Pattern => U ("[a-zA-Z][a-zA-Z0-9_]*")));

   Literals.Append (U ("+"));
   Literals.Append (U ("-"));
   Literals.Append (U (" "));

   Result := Lex
     (Source         => "Test + Who - Man",
      Tokens         => Tokens,
      Partial_Tokens => Partial_Tokens,
      Literals       => Literals,
      Ignore         => " " & ASCII.HT & ASCII.LF);

   for T of Result loop
      Print_Token (T);
   end loop;

   ---------------------------------------------------------------
   --  Test 2: OPTIMADE filter expression
   ---------------------------------------------------------------
   Tokens.Clear;
   Partial_Tokens.Clear;
   Literals.Clear;

   --  Keyword literals (order matches Python test)
   Literals.Append (U ("AND"));
   Literals.Append (U ("NOT"));
   Literals.Append (U ("OR"));
   Literals.Append (U ("KNOWN"));
   Literals.Append (U ("UNKNOWN"));
   Literals.Append (U ("IS"));
   Literals.Append (U ("CONTAINS"));
   Literals.Append (U ("STARTS"));
   Literals.Append (U ("ENDS"));
   Literals.Append (U ("WITH"));
   Literals.Append (U ("LENGTH"));
   Literals.Append (U ("HAS"));
   Literals.Append (U ("ALL"));
   Literals.Append (U ("ONLY"));
   Literals.Append (U ("EXACTLY"));
   Literals.Append (U ("ANY"));
   Literals.Append (U (" "));
   Literals.Append (U ((1 => ASCII.HT)));
   Literals.Append (U ((1 => ASCII.LF)));
   Literals.Append (U ((1 => ASCII.CR)));

   --  Token regexes (order matches Python test)
   Tokens.Append ((Name    => U ("Operator"),
                    Pattern => U ("<|<=|>|>=|=|!=")));
   Tokens.Append ((Name    => U ("Identifier"),
                    Pattern => U ("[a-z_][a-z_0-9]*")));
   --  String pattern: Python (?:...) → POSIX (...)
   Tokens.Append ((Name    => U ("String"),
                    Pattern => U ("""[^""\\]*(\\.[^""\\]*)*""")));
   Tokens.Append ((Name    => U ("Number"),
                    Pattern =>
                      U ("[-+]?([0-9]+(\.[0-9]*)?|\.[0-9]+)"
                         & "([eE][-+]?[0-9]+)?")));
   Tokens.Append ((Name    => U ("OpeningBrace"),
                    Pattern => U ("\(")));
   Tokens.Append ((Name    => U ("ClosingBrace"),
                    Pattern => U ("\)")));
   Tokens.Append ((Name    => U ("Dot"),
                    Pattern => U ("\.")));
   Tokens.Append ((Name    => U ("Colon"),
                    Pattern => U (":")));
   Tokens.Append ((Name    => U ("Comma"),
                    Pattern => U (",")));

   --  Partial token regexes
   Partial_Tokens.Append
     ((Name    => U ("Number"),
       Pattern => U ("[-+]?[0-9]+\.?[0-9]*[eE]?[-+]?[0-9]*")));

   Result := Lex
     (Source         =>
        "elements HAS ALL ""Ga"",""Ti"""
        & " AND (nelements=3 OR nelements=2)",
      Tokens         => Tokens,
      Partial_Tokens => Partial_Tokens,
      Literals       => Literals,
      Ignore         => " " & ASCII.HT & ASCII.LF);

   for T of Result loop
      Print_Token (T);
   end loop;

end Test_Lexer;
