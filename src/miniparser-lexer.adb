with Ada.Text_IO;
with Ada.Strings.Fixed;
with GNAT.Regpat;

package body Miniparser.Lexer is

   -----------------------
   -- Full_Match: test whether Data matches Pattern as a complete string.
   -- Wraps the pattern in ^(...)$ anchors so partial matches are rejected.
   -- Returns False (rather than raising) if the pattern is invalid.
   -----------------------

   function Full_Match (Pattern : String; Data : String) return Boolean is
      use GNAT.Regpat;
      Anchored : constant String := "^(" & Pattern & ")$";
      Re       : constant Pattern_Matcher := Compile (Anchored);
      Matches  : Match_Array (0 .. 0);
   begin
      Match (Re, Data, Matches);
      return Matches (0) /= No_Match;
   exception
      when Expression_Error =>
         return False;
   end Full_Match;

   -----------------------
   -- Is_In_Ignore: check if Token appears as a substring of Ignore.
   -- Replicates Python's `token in ignore` substring test.  Used to
   -- decide whether a matched token should be withheld from output.
   -----------------------

   function Is_In_Ignore (Token : String; Ignore : String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Ignore, Token) > 0;
   end Is_In_Ignore;

   -----------------------
   -- Prescan_Source: split Source into individual characters with position
   -- info (line number, column, line text), stripping any regions delimited
   -- by Comment_Markers.  This is the Ada equivalent of Python's
   -- _split_chars_strip_comments generator.  The fast path (no comment
   -- markers) avoids marker checking entirely.
   -----------------------

   type Char_With_Pos is record
      Ch  : Character;
      Pos : Source_Position;
   end record;

   package Char_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Char_With_Pos);

   --  Matches_At: check if Source starting at position Idx matches the
   --  string Pat exactly.  Used for detecting comment start/end markers.
   function Matches_At
     (Source : String; Idx : Positive; Pat : String) return Boolean
   is
   begin
      if Idx + Pat'Length - 1 > Source'Last then
         return False;
      end if;
      return Source (Idx .. Idx + Pat'Length - 1) = Pat;
   end Matches_At;

   procedure Prescan_Source
     (Source          : String;
      Comment_Markers : Comment_Vectors.Vector;
      Chars           : in out Char_Vectors.Vector)
   is
      Line_Num   : Natural := 0;
      Line_Start : Natural := Source'First;
      Line_End   : Natural;

      --  Comment tracking state (persists across lines)
      In_Comment : Boolean := False;
      End_Mark   : Unbounded_String;
   begin
      if Source'Length = 0 then
         return;
      end if;

      Line_Start := Source'First;
      while Line_Start <= Source'Last loop
         Line_Num := Line_Num + 1;

         --  Find end of line (position of LF, or past Source'Last)
         Line_End := Line_Start;
         while Line_End <= Source'Last
           and then Source (Line_End) /= ASCII.LF
         loop
            Line_End := Line_End + 1;
         end loop;
         --  Line_End is at LF or at Source'Last + 1

         --  Compute line text (without trailing LF; Python: line.rstrip('\n'))
         declare
            Text_Last : constant Natural :=
              (if Line_End <= Source'Last
               then Line_End - 1
               else Source'Last);
            Line_Text : constant Unbounded_String :=
              (if Text_Last >= Line_Start
               then To_Unbounded_String (Source (Line_Start .. Text_Last))
               else Null_Unbounded_String);
            Char_Last : constant Natural :=
              (if Line_End <= Source'Last
               then Line_End       --  include the LF character
               else Source'Last);
         begin
            if Comment_Markers.Is_Empty then
               --  Fast path: no comment markers, emit all characters
               declare
                  Col : Natural := 0;
               begin
                  for J in Line_Start .. Char_Last loop
                     Col := Col + 1;
                     Chars.Append
                       ((Ch  => Source (J),
                         Pos => (Line      => Line_Num,
                                 Column    => Col,
                                 Line_Text => Line_Text)));
                  end loop;
               end;
            else
               --  Process line with comment stripping
               declare
                  J : Natural := Line_Start;
               begin
                  while J <= Char_Last loop
                     if In_Comment then
                        --  Look for end marker
                        declare
                           EM : constant String := To_String (End_Mark);
                        begin
                           if Matches_At (Source, J, EM) then
                              J := J + EM'Length;
                              In_Comment := False;
                           else
                              J := J + 1;
                           end if;
                        end;
                     else
                        --  Check for start of a comment
                        declare
                           Found : Boolean := False;
                        begin
                           for CM of Comment_Markers loop
                              declare
                                 SM : constant String :=
                                   To_String (CM.Start_Mark);
                              begin
                                 if Matches_At (Source, J, SM) then
                                    In_Comment := True;
                                    End_Mark := CM.End_Mark;
                                    J := J + SM'Length;
                                    Found := True;
                                    exit;
                                 end if;
                              end;
                           end loop;

                           if not Found then
                              --  Emit this character
                              declare
                                 Col : constant Natural :=
                                   J - Line_Start + 1;
                              begin
                                 Chars.Append
                                   ((Ch  => Source (J),
                                     Pos => (Line      => Line_Num,
                                             Column    => Col,
                                             Line_Text => Line_Text)));
                              end;
                              J := J + 1;
                           end if;
                        end;
                     end if;
                  end loop;
               end;
            end if;
         end;

         --  Advance past this line
         if Line_End <= Source'Last then
            Line_Start := Line_End + 1;  --  skip LF
         else
            exit;
         end if;
      end loop;
   end Prescan_Source;

   ---------
   -- Lex --
   ---------

   function Lex
     (Source          : String;
      Tokens          : Token_Def_Vectors.Vector;
      Partial_Tokens  : Token_Def_Vectors.Vector;
      Literals        : UString_Vectors.Vector;
      Ignore          : String;
      Comment_Markers : Comment_Vectors.Vector := Comment_Vectors.Empty_Vector;
      Verbosity       : Natural := 0) return Token_Entry_Vectors.Vector
   is
      Result : Token_Entry_Vectors.Vector;

      --  Pre-scanned characters
      Chars      : Char_Vectors.Vector;
      Char_Index : Natural := 0;

      --  Build combined literal set: Literals union individual Ignore chars
      All_Literals : UString_Vectors.Vector;

      --  Build ordered list of all token regex names:
      --  tokens first, then partial_tokens not already in tokens.
      type Token_Regex_Entry is record
         Name            : Unbounded_String;
         Has_Full        : Boolean := False;
         Full_Pattern    : Unbounded_String;
         Has_Partial     : Boolean := False;
         Partial_Pattern : Unbounded_String;
      end record;

      package TRE_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Token_Regex_Entry);
      All_Token_Regexes : TRE_Vectors.Vector;

      --  Lexer state
      Pushback       : Unbounded_String := Null_Unbounded_String;
      Stack          : Unbounded_String := Null_Unbounded_String;

      Has_Seen_Token : Boolean := False;
      Seen_Token     : Unbounded_String;
      Seen_Token_Pos : Source_Position;
      Seen_Token_Len : Natural := 0;

      Last_Good_Pos       : Source_Position := (0, 0, Null_Unbounded_String);
      Last_Good_Pos_Next  : Source_Position := (0, 0, Null_Unbounded_String);
      LGPN_Is_Null        : Boolean := False;

      Pos     : Source_Position := (0, 0, Null_Unbounded_String);
      C       : Character;
      C_Empty : Boolean := False;

      --  Helper: find a token regex entry by name
      function Find_TRE (Name : Unbounded_String) return Natural is
      begin
         for I in 1 .. Natural (All_Token_Regexes.Length) loop
            if All_Token_Regexes (I).Name = Name then
               return I;
            end if;
         end loop;
         return 0;
      end Find_TRE;

   begin
      --  Pre-scan source into characters with positions
      Prescan_Source (Source, Comment_Markers, Chars);

      --  Build All_Literals = Literals union {each char of Ignore}
      All_Literals := Literals.Copy;
      for I in Ignore'Range loop
         declare
            Ignore_Str : constant Unbounded_String :=
              To_Unbounded_String (Ignore (I .. I));
            Found : Boolean := False;
         begin
            for J in 1 .. Natural (All_Literals.Length) loop
               if All_Literals (J) = Ignore_Str then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               All_Literals.Append (Ignore_Str);
            end if;
         end;
      end loop;

      --  Build All_Token_Regexes: tokens first, then unique partial_tokens
      for T of Tokens loop
         All_Token_Regexes.Append
           ((Name            => T.Name,
             Has_Full        => True,
             Full_Pattern    => T.Pattern,
             Has_Partial     => False,
             Partial_Pattern => Null_Unbounded_String));
      end loop;

      for PT of Partial_Tokens loop
         declare
            Idx : constant Natural := Find_TRE (PT.Name);
         begin
            if Idx > 0 then
               --  Update existing entry
               declare
                  E : Token_Regex_Entry := All_Token_Regexes (Idx);
               begin
                  E.Has_Partial := True;
                  E.Partial_Pattern := PT.Pattern;
                  All_Token_Regexes.Replace_Element (Idx, E);
               end;
            else
               All_Token_Regexes.Append
                 ((Name            => PT.Name,
                   Has_Full        => False,
                   Full_Pattern    => Null_Unbounded_String,
                   Has_Partial     => True,
                   Partial_Pattern => PT.Pattern));
            end if;
         end;
      end loop;

      --  Main lexer loop — greedy tokenization
      --
      --  On each iteration, one character is consumed (from pushback or from
      --  the pre-scanned source) and appended to the accumulation stack.  The
      --  stack is then tested against all literals and token regexes.  If a
      --  full match is found, it is recorded as the "seen token" (the best
      --  match so far).  If a partial match is found, accumulation continues.
      --  If neither matches, the last recorded full match is emitted and any
      --  unmatched remainder is pushed back for re-processing.
      --
      --  Python: while c != '' or len(pushback)>0 or seen_token is not None
      --  We use C_Empty=True to represent c=='' (initially False since
      --  c starts as None in Python, and None != '' is True)
      C_Empty := False;

      loop
         --  Loop exit: equivalent to Python's while condition being false
         --  But on first iteration, C_Empty is False (representing Python's c=None)
         --  After the first character read, C_Empty reflects actual state
         exit when C_Empty
           and then Length (Pushback) = 0
           and then not Has_Seen_Token;

         --  Get next character
         if Length (Pushback) > 0 then
            C := Element (Pushback, 1);
            Delete (Pushback, 1, 1);
            C_Empty := False;
         elsif Char_Index < Natural (Chars.Length) then
            Char_Index := Char_Index + 1;
            C := Chars (Char_Index).Ch;
            Pos := Chars (Char_Index).Pos;
            C_Empty := False;
         else
            C_Empty := True;
         end if;

         --  Append to stack (no-op when C_Empty, like Python's stack += '')
         if not C_Empty then
            Append (Stack, C);
         end if;

         if Verbosity >= 5 and then not C_Empty then
            Ada.Text_IO.Put_Line ("LEX INPUT:'" & C & "'");
         end if;

         --  Track position after last good match
         if LGPN_Is_Null then
            Last_Good_Pos_Next := Pos;
            LGPN_Is_Null := False;
         end if;

         --  Try to find a match
         declare
            Found     : Boolean := False;
            Stack_Str : constant String := To_String (Stack);
         begin
            if not C_Empty then
               --  Check literals union ignore
               for L of All_Literals loop
                  if Stack_Str = To_String (L) then
                     Seen_Token     := L;
                     Seen_Token_Pos := Pos;
                     Seen_Token_Len := Length (L);
                     Has_Seen_Token := True;
                     Last_Good_Pos  := Pos;
                     LGPN_Is_Null   := True;
                     Found := True;
                     exit;
                  end if;
               end loop;

               --  Check token regexes (full and partial)
               if not Found then
                  for TRE of All_Token_Regexes loop
                     if TRE.Has_Full
                       and then Full_Match
                         (To_String (TRE.Full_Pattern), Stack_Str)
                     then
                        Seen_Token     := TRE.Name;
                        Seen_Token_Pos := Pos;
                        Seen_Token_Len := Length (Stack);
                        Has_Seen_Token := True;
                        Last_Good_Pos  := Pos;
                        LGPN_Is_Null   := True;
                        Found := True;
                        exit;
                     elsif TRE.Has_Partial
                       and then Full_Match
                         (To_String (TRE.Partial_Pattern), Stack_Str)
                     then
                        --  Partial match only: keep accumulating
                        Found := True;
                        exit;
                     end if;
                  end loop;
               end if;
            end if;

            --  If no match found (or C_Empty), yield pending token
            if not Found then
               if Has_Seen_Token then
                  declare
                     Token_Str : constant String :=
                       To_String (Seen_Token);
                  begin
                     if not Is_In_Ignore (Token_Str, Ignore) then
                        if Verbosity >= 4 then
                           Ada.Text_IO.Put_Line
                             ("LEX YIELD: ("
                              & Token_Str & ", "
                              & Slice (Stack, 1, Seen_Token_Len)
                              & ")");
                        end if;
                        Result.Append
                          ((Name   => Seen_Token,
                            Value  =>
                              To_Unbounded_String
                                (Slice (Stack, 1, Seen_Token_Len)),
                            Pos    => Seen_Token_Pos,
                            Is_End => False));
                     end if;
                  end;

                  --  Push back unmatched remainder
                  if Seen_Token_Len < Length (Stack) then
                     Pushback :=
                       To_Unbounded_String
                         (Slice (Stack, Seen_Token_Len + 1, Length (Stack)))
                       & Pushback;
                  end if;
                  Stack          := Null_Unbounded_String;
                  Has_Seen_Token := False;
                  Seen_Token_Len := 0;
               end if;
            end if;
         end;
      end loop;

      --  Check for leftover unmatched input
      if Length (Stack) > 0 then
         declare
            Err_Pos : Source_Position;
         begin
            if not LGPN_Is_Null then
               Err_Pos := Last_Good_Pos_Next;
            else
               Err_Pos := Last_Good_Pos;
            end if;
            raise Parser_Syntax_Error with
              "Parser lexing error: Unrecognized symbol starting at line: "
              & Natural'Image (Err_Pos.Line)
              & ", pos:" & Natural'Image (Err_Pos.Column)
              & ":" & ASCII.LF
              & To_String (Err_Pos.Line_Text) & ASCII.LF
              & (1 .. Err_Pos.Column - 1 => ' ') & "^";
         end;
      end if;

      --  End-of-input marker
      if Verbosity >= 4 then
         Ada.Text_IO.Put_Line ("LEX YIELD: (None, None)");
      end if;
      Result.Append
        ((Name   => Null_Unbounded_String,
          Value  => Null_Unbounded_String,
          Pos    => Pos,
          Is_End => True));

      return Result;
   end Lex;

end Miniparser.Lexer;
