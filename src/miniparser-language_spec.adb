with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Ordered_Sets;
with Miniparser.Parser;

package body Miniparser.Language_Spec is

   function U (S : String) return Unbounded_String
     renames To_Unbounded_String;

   function S (U : Unbounded_String) return String
     renames To_String;

   --  "" represents Python's None (end-of-input / epsilon)
   Null_Sym : constant Unbounded_String := Null_Unbounded_String;

   --  Sentinel for the start item's False lookahead
   False_LA : constant Unbounded_String := U ("__FALSE__");

   ----------------------------------------------------------------
   --  AST constructors
   ----------------------------------------------------------------

   function N (Tag : String) return AST_Node_Access is
   begin
      return new AST_Node'(Tag => U (Tag), Children => AST_Vectors.Empty_Vector);
   end N;

   function N (Tag : String; C1 : AST_Node_Access) return AST_Node_Access is
      R : constant AST_Node_Access := N (Tag);
   begin
      R.Children.Append (C1);
      return R;
   end N;

   function N (Tag : String; C1, C2 : AST_Node_Access) return AST_Node_Access is
      R : constant AST_Node_Access := N (Tag);
   begin
      R.Children.Append (C1);
      R.Children.Append (C2);
      return R;
   end N;

   function N (Tag : String; C1, C2, C3 : AST_Node_Access)
     return AST_Node_Access
   is
      R : constant AST_Node_Access := N (Tag);
   begin
      R.Children.Append (C1);
      R.Children.Append (C2);
      R.Children.Append (C3);
      return R;
   end N;

   function N (Tag : String; C1, C2, C3, C4 : AST_Node_Access)
     return AST_Node_Access
   is
      R : constant AST_Node_Access := N (Tag);
   begin
      R.Children.Append (C1);
      R.Children.Append (C2);
      R.Children.Append (C3);
      R.Children.Append (C4);
      return R;
   end N;

   function N (Tag : String; C1, C2, C3, C4, C5, C6, C7, C8 : AST_Node_Access)
     return AST_Node_Access
   is
      R : constant AST_Node_Access := N (Tag);
   begin
      R.Children.Append (C1);
      R.Children.Append (C2);
      R.Children.Append (C3);
      R.Children.Append (C4);
      R.Children.Append (C5);
      R.Children.Append (C6);
      R.Children.Append (C7);
      R.Children.Append (C8);
      return R;
   end N;

   ----------------------------------------------------------------
   --  AST comparison
   ----------------------------------------------------------------

   function AST_Equal (A, B : AST_Node_Access) return Boolean is
   begin
      if A = null and B = null then
         return True;
      end if;
      if A = null or B = null then
         return False;
      end if;
      if A.Tag /= B.Tag then
         return False;
      end if;
      if Natural (A.Children.Length) /= Natural (B.Children.Length) then
         return False;
      end if;
      for I in 1 .. Natural (A.Children.Length) loop
         if not AST_Equal (A.Children (I), B.Children (I)) then
            return False;
         end if;
      end loop;
      return True;
   end AST_Equal;

   ----------------------------------------------------------------
   --  AST debug printing
   ----------------------------------------------------------------

   procedure Print_AST (Node : AST_Node_Access; Indent : Natural := 0) is
      use Ada.Text_IO;
      Prefix : constant String := (1 .. Indent * 2 => ' ');
   begin
      if Node = null then
         Put_Line (Prefix & "(null)");
         return;
      end if;
      if Node.Children.Is_Empty then
         Put_Line (Prefix & "'" & S (Node.Tag) & "'");
      else
         Put_Line (Prefix & "('" & S (Node.Tag) & "'");
         for C of Node.Children loop
            Print_AST (C, Indent + 1);
         end loop;
         Put_Line (Prefix & ")");
      end if;
   end Print_AST;

   ----------------------------------------------------------------
   --  Translate_Regex_Escapes: GNAT.Regpat compatibility shim
   ----------------------------------------------------------------
   --  GNAT.Regpat does not support \v, \f, or \xNN hex escapes that
   --  may appear in EBNF special sequences (inline regex definitions).
   --  This function translates them into literal Ada characters so that
   --  GNAT.Regpat can compile the pattern.  Other escape sequences
   --  (e.g. \t, \n, \d) are passed through unchanged.

   function Translate_Regex_Escapes (Pat : String) return String is
      Result : Unbounded_String;
      I      : Positive := Pat'First;

      function Hex_Digit (C : Character) return Natural is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
            when others      => return Natural'Last;  --  sentinel: not a hex digit
         end case;
      end Hex_Digit;

   begin
      while I <= Pat'Last loop
         if Pat (I) = '\' and then I + 1 <= Pat'Last then
            case Pat (I + 1) is
               when 'v' =>
                  Append (Result, Character'Val (11));  --  vertical tab
                  I := I + 2;
               when 'f' =>
                  Append (Result, Character'Val (12));  --  form feed
                  I := I + 2;
               when 'x' =>
                  --  \xNN hex escape
                  if I + 3 <= Pat'Last
                    and then Hex_Digit (Pat (I + 2)) /= Natural'Last
                    and then Hex_Digit (Pat (I + 3)) /= Natural'Last
                  then
                     Append (Result, Character'Val
                       (Hex_Digit (Pat (I + 2)) * 16
                        + Hex_Digit (Pat (I + 3))));
                     I := I + 4;
                  else
                     --  Not a valid \xNN, pass through as-is
                     Append (Result, Pat (I));
                     I := I + 1;
                  end if;
               when others =>
                  --  Pass through other escapes (\t, \n, \r, etc.)
                  Append (Result, Pat (I));
                  I := I + 1;
            end case;
         else
            Append (Result, Pat (I));
            I := I + 1;
         end if;
      end loop;
      return S (Result);
   end Translate_Regex_Escapes;

   ----------------------------------------------------------------
   --  Has_Token: check if a token name exists in the token definitions list.
   --  Used during EBNF-to-BNF conversion to skip grammar rules whose LHS
   --  is already defined as a token (regex-based terminal).
   ----------------------------------------------------------------

   function Has_Token
     (Toks : Token_Def_Vectors.Vector; Name : String) return Boolean
   is
   begin
      for T of Toks loop
         if S (T.Name) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Has_Token;

   --  In_Skip: check if Name appears in the skip-rules vector V.
   --  Used to exclude rules listed in Skip_Rules from BNF conversion.
   function In_Skip (V : UString_Vectors.Vector; Name : String) return Boolean is
   begin
      for X of V loop
         if S (X) = Name then
            return True;
         end if;
      end loop;
      return False;
   end In_Skip;

   ----------------------------------------------------------------
   --  EBNF_Unquote: process escape sequences in EBNF terminal strings.
   --  Handles \t (tab), \n (newline), and \r (carriage return).  Other
   --  backslash sequences are passed through with the backslash intact.
   --  The surrounding quote characters should already be stripped before
   --  calling this function.
   ----------------------------------------------------------------

   function EBNF_Unquote (Str : String) return String is
      Result : Unbounded_String;
      I : Natural := Str'First;
   begin
      while I <= Str'Last loop
         if I < Str'Last and then Str (I) = '\' then
            if Str (I + 1) = 't' then
               Append (Result, ASCII.HT);
               I := I + 2;
            elsif Str (I + 1) = 'n' then
               Append (Result, ASCII.LF);
               I := I + 2;
            elsif Str (I + 1) = 'r' then
               Append (Result, ASCII.CR);
               I := I + 2;
            else
               Append (Result, Str (I));
               I := I + 1;
            end if;
         else
            Append (Result, Str (I));
            I := I + 1;
         end if;
      end loop;
      return S (Result);
   end EBNF_Unquote;

   ----------------------------------------------------------------
   --  EBNF to BNF conversion
   --
   --  EBNF_To_BNF_RHS recursively processes an EBNF AST node (the RHS of
   --  a grammar rule) and returns a list of BNF alternatives (each is a
   --  vector of symbol names).  It handles:
   --    - identifier     → single symbol reference
   --    - terminal       → literal string (quotes stripped, escapes processed)
   --    - special        → inline regex (becomes a new token definition)
   --    - Concatenation  → Cartesian product of child alternatives
   --    - Alteration     → union of child alternatives
   --    - Optional       → child alternatives + epsilon
   --    - Grouping       → transparent (just recurse)
   --    - Repetition     → generates auxiliary "_repeat_*" rules
   --
   --  EBNF_To_BNF is the top-level driver that iterates over all Rule
   --  children of the Grammar AST, calling EBNF_To_BNF_RHS for each,
   --  and then converts any ?-prefixed rules into token definitions.
   ----------------------------------------------------------------

   --  Return type for EBNF_To_BNF_RHS: list of RHS alternatives.
   --  Each alternative is a UString_Vectors.Vector of symbol names.
   package Alt_Lists is new Ada.Containers.Vectors
     (Positive, UString_Vectors.Vector, UString_Vectors."=");

   function Vec_Of (Sym : String) return UString_Vectors.Vector is
      V : UString_Vectors.Vector;
   begin
      if Sym'Length > 0 then
         V.Append (U (Sym));
      end if;
      return V;
   end Vec_Of;

   function EBNF_To_BNF_RHS
     (RHS_Node  : AST_Node_Access;
      BNF       : in out BNF_Vectors.Vector;
      LHS_Name  : String;
      Lits      : String_Sets.Set) return Alt_Lists.Vector
   is
      Op   : constant String := S (RHS_Node.Tag);
      Alts : Alt_Lists.Vector;
   begin
      --  Check if the tag is a literal (e.g., brackets in EBNF)
      if Lits.Contains (Op) then
         declare
            V : UString_Vectors.Vector;
         begin
            V.Append (RHS_Node.Tag);
            Alts.Append (V);
            return Alts;
         end;
      end if;

      if Op = "identifier" then
         Alts.Append (Vec_Of (S (RHS_Node.Children (1).Tag)));
         return Alts;

      elsif Op = "special" then
         declare
            Raw : constant String := S (RHS_Node.Children (1).Tag);
            --  Strip surrounding ? marks and whitespace
            Content : constant String :=
              Ada.Strings.Fixed.Trim
                (Raw (Raw'First + 1 .. Raw'Last - 1),
                 Ada.Strings.Both);
            I : Positive := 1;
            Rep_Str : Unbounded_String;
         begin
            loop
               Rep_Str := U ("_special_" & LHS_Name & "_"
                             & Ada.Strings.Fixed.Trim
                               (Positive'Image (I), Ada.Strings.Both));
               declare
                  Found : Boolean := False;
               begin
                  for R of BNF loop
                     if S (R.LHS) = S (Rep_Str)
                       or else S (R.LHS) = "?" & S (Rep_Str)
                     then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  exit when not Found;
               end;
               I := I + 1;
            end loop;
            BNF.Append ((LHS => U ("?" & S (Rep_Str)),
                         RHS => Vec_Of (Translate_Regex_Escapes (Content))));
            Alts.Append (Vec_Of (S (Rep_Str)));
            return Alts;
         end;

      elsif Op = "terminal" then
         declare
            Raw : constant String := S (RHS_Node.Children (1).Tag);
            Content : constant String := Raw (Raw'First + 1 .. Raw'Last - 1);
         begin
            Alts.Append (Vec_Of (EBNF_Unquote (Content)));
            return Alts;
         end;

      elsif Op = "Concatenation" then
         --  Cartesian product of children alternatives, flattened
         declare
            Children_Alts : Alt_Lists.Vector;
            Temp : Alt_Lists.Vector;
         begin
            --  Start with one empty alternative
            Children_Alts.Append (UString_Vectors.Empty_Vector);

            for CI in 1 .. Natural (RHS_Node.Children.Length) loop
               declare
                  Child_Alts : constant Alt_Lists.Vector :=
                    EBNF_To_BNF_RHS
                      (RHS_Node.Children (CI), BNF, LHS_Name, Lits);
               begin
                  Temp.Clear;
                  for Acc of Children_Alts loop
                     for CA of Child_Alts loop
                        declare
                           Combined : UString_Vectors.Vector := Acc.Copy;
                        begin
                           for Sym of CA loop
                              if Length (Sym) > 0 then
                                 Combined.Append (Sym);
                              end if;
                           end loop;
                           Temp.Append (Combined);
                        end;
                     end loop;
                  end loop;
                  Children_Alts := Temp;
               end;
            end loop;
            return Children_Alts;
         end;

      elsif Op = "Alteration" then
         for CI in 1 .. Natural (RHS_Node.Children.Length) loop
            declare
               Child_Alts : constant Alt_Lists.Vector :=
                 EBNF_To_BNF_RHS
                   (RHS_Node.Children (CI), BNF, LHS_Name, Lits);
            begin
               for A of Child_Alts loop
                  Alts.Append (A);
               end loop;
            end;
         end loop;
         return Alts;

      elsif Op = "Optional" then
         Alts := EBNF_To_BNF_RHS
                   (RHS_Node.Children (1), BNF, LHS_Name, Lits);
         Alts.Append (UString_Vectors.Empty_Vector);  --  epsilon
         return Alts;

      elsif Op = "Grouping" then
         return EBNF_To_BNF_RHS
                  (RHS_Node.Children (1), BNF, LHS_Name, Lits);

      elsif Op = "Repetition" then
         declare
            I : Positive := 1;
            Rep_Str : Unbounded_String;
            Child_Alts : constant Alt_Lists.Vector :=
              EBNF_To_BNF_RHS
                (RHS_Node.Children (1), BNF, LHS_Name, Lits);
         begin
            loop
               Rep_Str := U ("_repeat_" & LHS_Name & "_"
                             & Ada.Strings.Fixed.Trim
                               (Positive'Image (I), Ada.Strings.Both));
               declare
                  Found : Boolean := False;
               begin
                  for R of BNF loop
                     if S (R.LHS) = S (Rep_Str) then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  exit when not Found;
               end;
               I := I + 1;
            end loop;

            for NR of Child_Alts loop
               declare
                  RHS_With_Repeat : UString_Vectors.Vector := NR.Copy;
                  RHS_Without     : constant UString_Vectors.Vector := NR.Copy;
               begin
                  RHS_With_Repeat.Append (Rep_Str);
                  BNF.Append ((LHS => Rep_Str, RHS => RHS_With_Repeat));
                  BNF.Append ((LHS => Rep_Str, RHS => RHS_Without));
               end;
            end loop;

            Alts.Append (Vec_Of (S (Rep_Str)));
            Alts.Append (UString_Vectors.Empty_Vector);  --  epsilon
            return Alts;
         end;

      else
         raise Parser_Grammar_Error with
           "Unrecognized symbol in grammar AST: " & Op;
      end if;
   end EBNF_To_BNF_RHS;

   procedure EBNF_To_BNF
     (AST   : AST_Node_Access;
      Toks  : in out Token_Def_Vectors.Vector;
      Skips : UString_Vectors.Vector;
      Lits  : String_Sets.Set;
      BNF   : out BNF_Vectors.Vector)
   is
   begin
      BNF.Clear;
      --  AST.Tag should be "Grammar"
      for CI in 1 .. Natural (AST.Children.Length) loop
         declare
            Rule_Node : constant AST_Node_Access := AST.Children (CI);
            LHS_Node  : constant AST_Node_Access := Rule_Node.Children (1);
            Symbol    : constant String := S (LHS_Node.Children (1).Tag);
            RHS_Node  : constant AST_Node_Access := Rule_Node.Children (2);
         begin
            if Has_Token (Toks, Symbol) or else In_Skip (Skips, Symbol) then
               null;  --  skip
            else
               declare
                  New_RHS_List : constant Alt_Lists.Vector :=
                    EBNF_To_BNF_RHS (RHS_Node, BNF, Symbol, Lits);
               begin
                  for RHS of New_RHS_List loop
                     BNF.Append ((LHS => U (Symbol), RHS => RHS));
                  end loop;
               end;
            end if;
         end;
      end loop;

      --  Convert ?-prefixed rules to token definitions
      for R of BNF loop
         declare
            Name : constant String := S (R.LHS);
         begin
            if Name'Length > 0 and then Name (Name'First) = '?' then
               declare
                  Token_Name : constant String :=
                    Name (Name'First + 1 .. Name'Last);
                  Pattern : constant String :=
                    (if Natural (R.RHS.Length) > 0
                     then S (R.RHS (1))
                     else "");
               begin
                  Toks.Append ((Name    => U (Token_Name),
                                Pattern => U (Pattern)));
               end;
            end if;
         end;
      end loop;

      --  Remove ?-prefixed rules
      declare
         Filtered : BNF_Vectors.Vector;
      begin
         for R of BNF loop
            declare
               Name : constant String := S (R.LHS);
            begin
               if Name'Length = 0
                 or else Name (Name'First) /= '?'
               then
                  Filtered.Append (R);
               end if;
            end;
         end loop;
         BNF := Filtered;
      end;
   end EBNF_To_BNF;

   ----------------------------------------------------------------
   --  Build_Rule_Table: construct a map from each non-terminal symbol to
   --  its list of RHS alternatives.  Terminal symbols and skipped rules
   --  are excluded.  This is step 3 of the 5-step build pipeline.
   ----------------------------------------------------------------

   procedure Build_Rule_Table
     (BNF       : BNF_Vectors.Vector;
      Terminals : String_Sets.Set;
      Skips     : UString_Vectors.Vector;
      RT        : out Rule_Maps.Map)
   is
   begin
      RT.Clear;
      for R of BNF loop
         declare
            LHS : constant String := S (R.LHS);
         begin
            if not Terminals.Contains (LHS)
              and then not In_Skip (Skips, LHS)
            then
               if not RT.Contains (LHS) then
                  RT.Insert (LHS, RHS_List.Empty_Vector);
               end if;
               RT (LHS).Append (R.RHS);
            end if;
         end;
      end loop;
   end Build_Rule_Table;

   ----------------------------------------------------------------
   --  Build_First_Table: compute FIRST sets via fixed-point iteration.
   --  For each symbol, FIRST(symbol) is the set of terminals that may
   --  appear as the first token when deriving from that symbol.  Terminals
   --  have FIRST = {self}.  Non-terminals accumulate FIRST sets from the
   --  first symbol of each of their RHS alternatives, iterating until no
   --  new terminals are added.  This is step 4 of the build pipeline.
   ----------------------------------------------------------------

   procedure Build_First_Table
     (RT        : Rule_Maps.Map;
      Terminals : String_Sets.Set;
      FT        : out First_Maps.Map)
   is
      Last_Count : Natural := 0;
   begin
      FT.Clear;

      --  Initialize: each terminal's FIRST set is itself
      for T of Terminals loop
         declare
            SS : String_Sets.Set;
         begin
            SS.Insert (T);
            FT.Insert (T, SS);
         end;
      end loop;

      --  Initialize: each non-terminal's FIRST set is empty
      for C in RT.Iterate loop
         declare
            Sym : constant String := Rule_Maps.Key (C);
         begin
            if not FT.Contains (Sym) then
               FT.Insert (Sym, String_Sets.Empty_Set);
            end if;
         end;
      end loop;

      --  Fixed-point iteration
      loop
         declare
            Count : Natural := 0;
         begin
            for C in RT.Iterate loop
               declare
                  Sym : constant String := Rule_Maps.Key (C);
                  Alts : constant RHS_List.Vector := Rule_Maps.Element (C);
               begin
                  for Alt of Alts loop
                     if Natural (Alt.Length) > 0 then
                        declare
                           First_Sym : constant String := S (Alt (1));
                        begin
                           if FT.Contains (First_Sym) then
                              for F of FT (First_Sym) loop
                                 FT (Sym).Include (F);
                              end loop;
                           end if;
                        end;
                     end if;
                  end loop;
                  Count := Count + Natural (FT (Sym).Length);
               end;
            end loop;

            exit when Count = Last_Count;
            Last_Count := Count;
         end;
      end loop;
   end Build_First_Table;

   ----------------------------------------------------------------
   --  Build Parse Tables (ACTION + GOTO) — step 5 of the build pipeline.
   --
   --  Constructs the LR(1) canonical collection of item sets and derives
   --  the ACTION and GOTO tables.  Uses Compute_Closure to expand item
   --  sets and resolves shift/reduce conflicts via the precedence table.
   --
   --  An LR(1) item is (LHS, RHS, Lookahead, Pos) where Pos is the
   --  position of the "dot" in the RHS.  The algorithm:
   --    1. Start with the augmented start item and compute its closure.
   --    2. For each unprocessed item set (state), group items by the
   --       symbol after the dot.
   --    3. For items with the dot at the end (Pos = RHS length), add a
   --       Reduce action keyed by the lookahead.
   --    4. For items with the dot before a symbol, advance the dot and
   --       compute the closure of the new item set.  If the symbol is a
   --       terminal, add a Shift action; if a non-terminal, add a GOTO.
   --    5. Shift/reduce conflicts are resolved by precedence/associativity;
   --       reduce/reduce conflicts raise Parser_Grammar_Error.
   ----------------------------------------------------------------

   --  LR Item: (LHS, RHS, Lookahead, Pos)
   --  LHS = "" for the augmented start rule
   --  Lookahead = "" for None/end-of-input, False_LA for start item
   type LR_Item is record
      LHS : Unbounded_String;
      RHS : UString_Vectors.Vector;
      LA  : Unbounded_String;
      Pos : Natural;
   end record;

   function Item_Less (L, R : LR_Item) return Boolean is
   begin
      if L.LHS < R.LHS then return True; end if;
      if L.LHS > R.LHS then return False; end if;
      if L.Pos < R.Pos then return True; end if;
      if L.Pos > R.Pos then return False; end if;
      if L.LA < R.LA then return True; end if;
      if L.LA > R.LA then return False; end if;
      declare
         LL : constant Natural := Natural (L.RHS.Length);
         RL : constant Natural := Natural (R.RHS.Length);
         ML : constant Natural := Natural'Min (LL, RL);
      begin
         for I in 1 .. ML loop
            if L.RHS (I) < R.RHS (I) then return True; end if;
            if L.RHS (I) > R.RHS (I) then return False; end if;
         end loop;
         return LL < RL;
      end;
   end Item_Less;

   package Item_Sets is new Ada.Containers.Ordered_Sets (LR_Item, Item_Less);

   --  Canonicalize an item set to a unique string for state lookup
   function Canonicalize (Items : Item_Sets.Set) return Unbounded_String is
      Result : Unbounded_String;
   begin
      for It of Items loop
         Append (Result, S (It.LHS));
         Append (Result, '|');
         for I in 1 .. Natural (It.RHS.Length) loop
            if I > 1 then Append (Result, ','); end if;
            Append (Result, S (It.RHS (I)));
         end loop;
         Append (Result, '|');
         Append (Result, S (It.LA));
         Append (Result, '|');
         Append (Result, Natural'Image (It.Pos));
         Append (Result, ';');
      end loop;
      return Result;
   end Canonicalize;

   --  Compute_Closure: expand an LR(1) item set by adding all items
   --  reachable by following non-terminal symbols after the dot.  For each
   --  item with the dot before a non-terminal B, adds items for every
   --  production of B, with lookaheads derived from the FIRST set of the
   --  symbols following B in the original item (or the item's own lookahead
   --  if B is the last symbol before the dot).  Iterates until no new
   --  items are added.
   function Compute_Closure
     (Initial   : Item_Sets.Set;
      RT        : Rule_Maps.Map;
      FT        : First_Maps.Map;
      Terminals : String_Sets.Set) return Item_Sets.Set
   is
      C : Item_Sets.Set := Initial.Copy;
      Changed : Boolean;
   begin
      loop
         Changed := False;
         declare
            Additions : Item_Sets.Set;
         begin
            for It of C loop
               declare
                  Len : constant Natural := Natural (It.RHS.Length);
               begin
                  if It.Pos < Len then
                     declare
                        Symbol : constant String := S (It.RHS (It.Pos + 1));
                        TS : String_Sets.Set;
                     begin
                        if It.Pos + 1 = Len then
                           --  Dot before last symbol: lookahead = item's LA
                           TS.Insert (S (It.LA));
                        else
                           --  Dot with more symbols after
                           declare
                              Next : constant String :=
                                S (It.RHS (It.Pos + 2));
                           begin
                              if not FT.Contains (Next) then
                                 raise Parser_Grammar_Error with
                                   "Symbol not in grammar or terminals: '"
                                   & Next & "'";
                              end if;
                              TS := FT (Next);
                           end;
                        end if;

                        if not Terminals.Contains (Symbol) then
                           if RT.Contains (Symbol) then
                              for P of RT (Symbol) loop
                                 for T of TS loop
                                    declare
                                       NI : constant LR_Item :=
                                         (LHS => U (Symbol),
                                          RHS => P,
                                          LA  => U (T),
                                          Pos => 0);
                                    begin
                                       if not C.Contains (NI)
                                         and then not Additions.Contains (NI)
                                       then
                                          Additions.Insert (NI);
                                          Changed := True;
                                       end if;
                                    end;
                                 end loop;
                              end loop;
                           end if;
                        end if;
                     end;
                  end if;
               end;
            end loop;

            for A of Additions loop
               C.Insert (A);
            end loop;
         end;

         exit when not Changed;
      end loop;
      return C;
   end Compute_Closure;

   procedure Build_Parse_Tables
     (RT        : Rule_Maps.Map;
      FT        : First_Maps.Map;
      Terminals : String_Sets.Set;
      Start_Sym : String;
      Prec      : Prec_Vectors.Vector;
      A_Tbl     : out Action_Maps.Map;
      G_Tbl     : out Goto_Maps.Map)
   is
      --  States table: canonical string -> state number
      package State_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String, Positive);
      States : State_Maps.Map;

      --  Todo list
      package ISV is new Ada.Containers.Vectors
        (Positive, Item_Sets.Set, Item_Sets."=");
      Todo : ISV.Vector;

      --  Precedence table: symbol -> (priority, associativity)
      type Prec_Info is record
         Priority : Natural;
         Assoc    : Unbounded_String;
      end record;
      package Prec_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String, Prec_Info);
      Prec_Table : Prec_Maps.Map;

      function Find_Or_Create (CL : Item_Sets.Set) return Positive is
         Key : constant String := S (Canonicalize (CL));
      begin
         if States.Contains (Key) then
            return States (Key);
         else
            declare
               St : constant Positive := Natural (States.Length) + 1;
            begin
               States.Insert (Key, St);
               return St;
            end;
         end if;
      end Find_Or_Create;

      type Prec_Result is (PR_Shift, PR_Reduce, PR_Empty, PR_Unknown);

      function Precedence_Check
        (Symbol : String; RHS : UString_Vectors.Vector) return Prec_Result
      is
         Found    : Natural := 0;
         Found_Sym : Unbounded_String;
      begin
         if not Prec_Table.Contains (Symbol) then
            return PR_Unknown;
         end if;
         for Sym of RHS loop
            if Prec_Table.Contains (S (Sym)) then
               Found := Found + 1;
               Found_Sym := Sym;
            end if;
         end loop;
         if Found = 1 then
            if Symbol = S (Found_Sym) then
               if S (Prec_Table (Symbol).Assoc) = "left" then
                  return PR_Reduce;
               elsif S (Prec_Table (Symbol).Assoc) = "right" then
                  return PR_Shift;
               else
                  return PR_Empty;
               end if;
            else
               if Prec_Table (Symbol).Priority
                 > Prec_Table (S (Found_Sym)).Priority
               then
                  return PR_Shift;
               else
                  return PR_Reduce;
               end if;
            end if;
         else
            return PR_Unknown;
         end if;
      end Precedence_Check;

      --  Item dictionary: groups items by the symbol after the dot
      package Item_Vec is new Ada.Containers.Vectors (Positive, LR_Item);
      package Item_Dict_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String, Item_Vec.Vector, "<", Item_Vec."=");

   begin
      A_Tbl.Clear;
      G_Tbl.Clear;

      --  Build precedence table
      for I in 1 .. Natural (Prec.Length) loop
         declare
            PE : constant Precedence_Entry := Prec (I);
         begin
            for Sym of PE.Symbols loop
               Prec_Table.Insert
                 (S (Sym), (Priority => I - 1, Assoc => PE.Assoc));
            end loop;
         end;
      end loop;

      --  Start item: (None, (Start, None), False, 0)
      declare
         Start_RHS : UString_Vectors.Vector;
         Start_Item : LR_Item;
         Start_Set : Item_Sets.Set;
         Start_CL : Item_Sets.Set;
      begin
         Start_RHS.Append (U (Start_Sym));
         Start_RHS.Append (Null_Sym);  --  None = end-of-input
         Start_Item := (LHS => Null_Sym,  --  None = augmented start
                        RHS => Start_RHS,
                        LA  => False_LA,
                        Pos => 0);
         Start_Set.Insert (Start_Item);
         Start_CL := Compute_Closure (Start_Set, RT, FT, Terminals);
         Todo.Append (Start_CL);
      end;

      --  Process all states
      while Natural (Todo.Length) > 0 loop
         declare
            CL : constant Item_Sets.Set :=
              Todo.Last_Element;
            State : constant Positive := Find_Or_Create (CL);
            Item_Dict : Item_Dict_Maps.Map;
         begin
            Todo.Delete_Last;

            if A_Tbl.Contains (State) and then G_Tbl.Contains (State) then
               goto Continue_State;
            end if;

            --  Group items by symbol after dot
            for It of CL loop
               declare
                  Len : constant Natural := Natural (It.RHS.Length);
                  Arg : Unbounded_String;
               begin
                  if Len > It.Pos then
                     Arg := It.RHS (It.Pos + 1);
                  else
                     Arg := Null_Sym;
                  end if;
                  declare
                     Key : constant String := S (Arg);
                  begin
                     if not Item_Dict.Contains (Key) then
                        Item_Dict.Insert (Key, Item_Vec.Empty_Vector);
                     end if;
                     Item_Dict (Key).Append (It);
                  end;
               end;
            end loop;

            A_Tbl.Insert (State, Action_Inner.Empty_Map);
            G_Tbl.Insert (State, Goto_Inner.Empty_Map);

            for DC in Item_Dict.Iterate loop
               declare
                  Symbol : constant String := Item_Dict_Maps.Key (DC);
                  Items  : constant Item_Vec.Vector :=
                    Item_Dict_Maps.Element (DC);
                  New_Items : Item_Sets.Set;
               begin
                  for It of Items loop
                     declare
                        Len : constant Natural := Natural (It.RHS.Length);
                     begin
                        if Len > It.Pos then
                           --  Dot before a symbol
                           if Symbol = "" then
                              --  symbol is None -> accept
                              A_Tbl (State).Include
                                ("", (Kind => Accept_Action, others => <>));
                           else
                              declare
                                 NI : constant LR_Item :=
                                   (LHS => It.LHS,
                                    RHS => It.RHS,
                                    LA  => It.LA,
                                    Pos => It.Pos + 1);
                              begin
                                 if not New_Items.Contains (NI) then
                                    New_Items.Insert (NI);
                                 end if;
                              end;
                           end if;
                        else
                           --  Dot at end -> reduce
                           declare
                              LA_Key : constant String := S (It.LA);
                           begin
                              if not A_Tbl (State).Contains (LA_Key) then
                                 A_Tbl (State).Insert
                                   (LA_Key,
                                    (Kind       => Reduce,
                                     New_State  => 0,
                                     Reduce_LHS => It.LHS,
                                     Reduce_RHS => It.RHS));
                              elsif A_Tbl (State)(LA_Key).Kind = Shift then
                                 declare
                                    Check : constant Prec_Result :=
                                      Precedence_Check (LA_Key, It.RHS);
                                 begin
                                    case Check is
                                       when PR_Reduce =>
                                          A_Tbl (State).Replace
                                            (LA_Key,
                                             (Kind       => Reduce,
                                              New_State  => 0,
                                              Reduce_LHS => It.LHS,
                                              Reduce_RHS => It.RHS));
                                       when PR_Shift =>
                                          null;
                                       when PR_Empty =>
                                          A_Tbl (State).Delete (LA_Key);
                                       when PR_Unknown =>
                                          null;  --  default: keep shift
                                    end case;
                                 end;
                              elsif A_Tbl (State)(LA_Key).Kind = Reduce then
                                 raise Parser_Grammar_Error with
                                   "reduce/reduce conflict in state"
                                   & Positive'Image (State)
                                   & " for symbol: " & LA_Key;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;

                  if Natural (New_Items.Length) > 0 then
                     declare
                        New_CL : constant Item_Sets.Set :=
                          Compute_Closure (New_Items, RT, FT, Terminals);
                        New_State : constant Positive :=
                          Find_Or_Create (New_CL);
                     begin
                        if Terminals.Contains (Symbol) then
                           if not A_Tbl (State).Contains (Symbol) then
                              A_Tbl (State).Insert
                                (Symbol,
                                 (Kind      => Shift,
                                  New_State => New_State,
                                  others    => <>));
                           elsif A_Tbl (State)(Symbol).Kind = Shift then
                              raise Parser_Grammar_Error with
                                "shift/shift conflict in state"
                                & Positive'Image (State)
                                & " for symbol: " & Symbol;
                           else
                              --  shift/reduce conflict
                              declare
                                 RHS_For_Check : constant
                                   UString_Vectors.Vector :=
                                     A_Tbl (State)(Symbol).Reduce_RHS;
                                 Check : constant Prec_Result :=
                                   Precedence_Check (Symbol, RHS_For_Check);
                              begin
                                 case Check is
                                    when PR_Shift =>
                                       A_Tbl (State).Replace
                                         (Symbol,
                                          (Kind      => Shift,
                                           New_State => New_State,
                                           others    => <>));
                                    when PR_Reduce =>
                                       null;
                                    when PR_Empty =>
                                       A_Tbl (State).Delete (Symbol);
                                    when PR_Unknown =>
                                       --  Default: shift wins
                                       A_Tbl (State).Replace
                                         (Symbol,
                                          (Kind      => Shift,
                                           New_State => New_State,
                                           others    => <>));
                                 end case;
                              end;
                           end if;
                        else
                           G_Tbl (State).Insert (Symbol, New_State);
                        end if;
                        Todo.Append (New_CL);
                     end;
                  end if;
               end;
            end loop;

            <<Continue_State>>
         end;
      end loop;
   end Build_Parse_Tables;

   ----------------------------------------------------------------
   --  Bootstrap: the hardcoded EBNF language spec
   --
   --  Initialize_LS_EBNF sets up the language specification for parsing
   --  EBNF grammars themselves.  It contains a hardcoded AST of the EBNF
   --  meta-grammar (the same one that could be produced by parsing the
   --  EBNF text, but provided as a pre-built AST to break the chicken-
   --  and-egg problem).  This is the bootstrap mechanism that allows the
   --  parser to parse user-provided EBNF grammars.
   ----------------------------------------------------------------

   LS_EBNF : Language_Spec_Record;
   LS_EBNF_Initialized : Boolean := False;

   procedure Initialize_LS_EBNF is
   begin
      if LS_EBNF_Initialized then
         return;
      end if;

      --  Hardcoded EBNF grammar AST (same as test_ls.adb / Python ls_ebnf)
      LS_EBNF.EBNF_AST := N ("Grammar",
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

      LS_EBNF.Start := U ("Grammar");

      LS_EBNF.Ignore.Insert (" ");
      LS_EBNF.Ignore.Insert ((1 => ASCII.HT));
      LS_EBNF.Ignore.Insert ((1 => ASCII.LF));

      LS_EBNF.Comment_Markers.Append
        ((Start_Mark => U ("(*"), End_Mark => U ("*)")));

      LS_EBNF.Literals.Insert ("[");
      LS_EBNF.Literals.Insert ("]");
      LS_EBNF.Literals.Insert ("{");
      LS_EBNF.Literals.Insert ("}");
      LS_EBNF.Literals.Insert ("(");
      LS_EBNF.Literals.Insert (")");
      LS_EBNF.Literals.Insert ("|");
      LS_EBNF.Literals.Insert (",");
      LS_EBNF.Literals.Insert (";");
      LS_EBNF.Literals.Insert ("=");

      declare
         P1 : Precedence_Entry;
         P2 : Precedence_Entry;
      begin
         P1.Assoc := U ("left");
         P1.Symbols.Append (U ("|"));
         P2.Assoc := U ("left");
         P2.Symbols.Append (U (","));
         LS_EBNF.Precedence.Append (P1);
         LS_EBNF.Precedence.Append (P2);
      end;

      LS_EBNF.Tokens.Append
        ((Name    => U ("identifier"),
          Pattern => U ("[a-zA-Z][a-zA-Z0-9_]*")));
      LS_EBNF.Tokens.Append
        ((Name    => U ("terminal"),
          Pattern => U ("""([^\\""]|.)*""|'([^\\']|.)*'")));
      LS_EBNF.Tokens.Append
        ((Name    => U ("special"),
          Pattern => U ("\?[^?]*\?")));

      LS_EBNF.Simplify.Append (U ("Rhs"));
      LS_EBNF.Aggregate.Append (U ("Grammar"));
      LS_EBNF.Aggregate.Append (U ("Alteration"));
      LS_EBNF.Aggregate.Append (U ("Concatenation"));
      LS_EBNF.Remove.Append (U ("["));
      LS_EBNF.Remove.Append (U ("]"));
      LS_EBNF.Remove.Append (U ("{"));
      LS_EBNF.Remove.Append (U ("}"));
      LS_EBNF.Remove.Append (U ("("));
      LS_EBNF.Remove.Append (U (")"));
      LS_EBNF.Remove.Append (U ("|"));
      LS_EBNF.Remove.Append (U (","));
      LS_EBNF.Remove.Append (U (";"));
      LS_EBNF.Remove.Append (U ("="));

      LS_EBNF_Initialized := True;
   end Initialize_LS_EBNF;

   ----------------------------------------------------------------
   --  Build_LS: main entry point
   ----------------------------------------------------------------

   procedure Build_LS
     (LS        : in out Language_Spec_Record;
      Verbosity : Natural := 0)
   is
      pragma Unreferenced (Verbosity);
   begin
      --  Step 1: Get BNF grammar from EBNF AST
      if not LS.Has_BNF then
         if LS.EBNF_AST = null then
            --  No pre-built AST: parse EBNF grammar text via bootstrap
            if Length (LS.EBNF_Grammar) = 0 then
               raise Parser_Grammar_Error with
                 "build_ls needs ebnf_grammar or ebnf_grammar_ast";
            end if;

            --  Initialize and build the bootstrap EBNF parser
            Initialize_LS_EBNF;
            if not LS_EBNF.Has_Parse then
               Build_LS (LS_EBNF);
            end if;

            --  Parse the EBNF grammar text
            LS.EBNF_AST :=
              Miniparser.Parser.Parse (LS_EBNF, S (LS.EBNF_Grammar));
         end if;

         --  Build a literal set for the EBNF converter (from the EBNF
         --  grammar's own literals, which are the LS.Literals)
         declare
            EBNF_Lits : String_Sets.Set;
         begin
            --  The EBNF converter needs to know which single-char tags
            --  are EBNF literals (brackets, etc.). These come from LS.Literals.
            for L of LS.Literals loop
               EBNF_Lits.Insert (L);
            end loop;
            EBNF_To_BNF
              (LS.EBNF_AST, LS.Tokens, LS.Skip_Rules, EBNF_Lits,
               LS.BNF_Grammar);
         end;
         LS.Has_BNF := True;
      end if;

      --  Step 2: Determine start symbol
      if Length (LS.Start) = 0 and then Natural (LS.BNF_Grammar.Length) > 0 then
         LS.Start := LS.BNF_Grammar (1).LHS;
      end if;

      --  Step 3: Build terminals set
      if not LS.Has_Terminals then
         LS.Terminals.Clear;
         for T of LS.Tokens loop
            LS.Terminals.Insert (S (T.Name));
         end loop;
         for L of LS.Literals loop
            LS.Terminals.Include (L);
         end loop;
         LS.Terminals.Include ("");  --  None / epsilon
         LS.Has_Terminals := True;
      end if;

      --  Step 4: Build rule table
      if not LS.Has_Rules then
         Build_Rule_Table
           (LS.BNF_Grammar, LS.Terminals, LS.Skip_Rules, LS.Rule_Table);
         LS.Has_Rules := True;
      end if;

      --  Step 5: Build FIRST table
      if not LS.Has_First then
         Build_First_Table (LS.Rule_Table, LS.Terminals, LS.First_Table);
         LS.Has_First := True;
      end if;

      --  Step 6: Build parse tables
      if not LS.Has_Parse then
         Build_Parse_Tables
           (LS.Rule_Table, LS.First_Table, LS.Terminals,
            S (LS.Start), LS.Precedence,
            LS.Action_Table, LS.Goto_Table);
         LS.Has_Parse := True;
      end if;
   end Build_LS;

   ----------------------------------------------------------------
   --  Print helpers
   ----------------------------------------------------------------

   procedure Print_LS (LS : Language_Spec_Record) is
      use Ada.Text_IO;

      procedure Put_Sym (Sym : String) is
      begin
         if Sym = "" then
            Put ("None");
         else
            Put ("'" & Sym & "'");
         end if;
      end Put_Sym;

      procedure Put_RHS (RHS : UString_Vectors.Vector) is
      begin
         Put ("(");
         for I in 1 .. Natural (RHS.Length) loop
            if I > 1 then Put (", "); end if;
            Put_Sym (S (RHS (I)));
         end loop;
         Put (")");
      end Put_RHS;
   begin
      --  BNF Grammar
      Put_Line ("=== BNF Grammar ===");
      for R of LS.BNF_Grammar loop
         Put ("  " & S (R.LHS) & " -> ");
         Put_RHS (R.RHS);
         New_Line;
      end loop;

      --  Rule Table
      Put_Line ("=== Rule Table ===");
      for C in LS.Rule_Table.Iterate loop
         declare
            Sym : constant String := Rule_Maps.Key (C);
            Alts : constant RHS_List.Vector := Rule_Maps.Element (C);
         begin
            Put ("  " & Sym & ": [");
            for I in 1 .. Natural (Alts.Length) loop
               if I > 1 then Put (", "); end if;
               Put_RHS (Alts (I));
            end loop;
            Put_Line ("]");
         end;
      end loop;

      --  First Table
      Put_Line ("=== First Table ===");
      for C in LS.First_Table.Iterate loop
         declare
            Sym : constant String := First_Maps.Key (C);
            FS  : constant String_Sets.Set := First_Maps.Element (C);
         begin
            Put ("  ");
            Put_Sym (Sym);
            Put (": {");
            declare
               First_Item : Boolean := True;
            begin
               for F of FS loop
                  if not First_Item then Put (", "); end if;
                  Put_Sym (F);
                  First_Item := False;
               end loop;
            end;
            Put_Line ("}");
         end;
      end loop;

      --  Terminals
      Put ("=== Terminals === {");
      declare
         First_Item : Boolean := True;
      begin
         for T of LS.Terminals loop
            if not First_Item then Put (", "); end if;
            Put_Sym (T);
            First_Item := False;
         end loop;
      end;
      Put_Line ("}");

      --  Action Table
      Put_Line ("=== Action Table ===");
      for SC in LS.Action_Table.Iterate loop
         declare
            St : constant Positive := Action_Maps.Key (SC);
            Inner : constant Action_Inner.Map := Action_Maps.Element (SC);
         begin
            Put ("  " & Positive'Image (St) & ": {");
            declare
               First_Entry : Boolean := True;
            begin
               for IC in Inner.Iterate loop
                  declare
                     Sym : constant String := Action_Inner.Key (IC);
                     Act : constant Action_Entry := Action_Inner.Element (IC);
                  begin
                     if not First_Entry then Put (", "); end if;
                     Put_Sym (Sym);
                     Put (": ");
                     case Act.Kind is
                        when Shift =>
                           Put ("('shift'," & Natural'Image (Act.New_State)
                                & ")");
                        when Reduce =>
                           Put ("('reduce', (" & S (Act.Reduce_LHS) & ", ");
                           Put_RHS (Act.Reduce_RHS);
                           Put ("))");
                        when Accept_Action =>
                           Put ("('accept', None)");
                     end case;
                     First_Entry := False;
                  end;
               end loop;
            end;
            Put_Line ("}");
         end;
      end loop;

      --  Goto Table
      Put_Line ("=== Goto Table ===");
      for SC in LS.Goto_Table.Iterate loop
         declare
            St : constant Positive := Goto_Maps.Key (SC);
            Inner : constant Goto_Inner.Map := Goto_Maps.Element (SC);
         begin
            Put ("  " & Positive'Image (St) & ": {");
            declare
               First_Entry : Boolean := True;
            begin
               for IC in Inner.Iterate loop
                  declare
                     Sym : constant String := Goto_Inner.Key (IC);
                     Tgt : constant Positive := Goto_Inner.Element (IC);
                  begin
                     if not First_Entry then Put (", "); end if;
                     Put ("'" & Sym & "':" & Positive'Image (Tgt));
                     First_Entry := False;
                  end;
               end loop;
            end;
            Put_Line ("}");
         end;
      end loop;
   end Print_LS;

end Miniparser.Language_Spec;
