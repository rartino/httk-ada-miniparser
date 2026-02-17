with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Miniparser.Lexer;         use Miniparser.Lexer;
with Miniparser.Language_Spec; use Miniparser.Language_Spec;
with Miniparser.Parser;

package body Optimadeparser.Parse is

   function U (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   function S (UStr : Unbounded_String) return String
     renames To_String;

   LS          : Language_Spec_Record;
   Initialized : Boolean := False;

   --  Read_File: read an entire text file into a single String.
   --  Lines are separated by LF characters in the result.
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Verbosity : Natural := 0) is
      Grammar_Path : constant String :=
        "grammars/optimade_filter_grammar.ebnf";
   begin
      if Initialized then
         return;
      end if;

      declare
         Grammar : constant String := Read_File (Grammar_Path);
      begin
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

      --  Token definitions
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

      --  Skip rules
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

      Build_LS (LS, Verbosity);
      Initialized := True;
   end Initialize;

   ---------------
   -- Parse_Raw --
   ---------------

   function Parse_Raw
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access
   is
   begin
      if not Initialized then
         Initialize (Verbosity);
      end if;
      return Miniparser.Parser.Parse (LS, Filter_String, Verbosity);
   end Parse_Raw;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filter_String : String;
      Verbosity     : Natural := 0) return AST_Node_Access
   is
      Raw : constant AST_Node_Access := Parse_Raw (Filter_String, Verbosity);
   begin
      return Parse_Tree_To_OJF (Raw);
   end Parse;

   ---------------
   -- Fix_Const --
   ---------------

   --  Fix_Const: normalize a Property, String, or Number node for OJF output.
   --    Property → returns the Identifier child directly.
   --    String   → strips surrounding double-quote characters from the value.
   --    Number (or other) → returned as-is.
   function Fix_Const (Node : AST_Node_Access) return AST_Node_Access is
      Tag : constant String := S (Node.Tag);
   begin
      if Tag = "Property" then
         --  Return the Identifier child
         return Node.Children (1);
      elsif Tag = "String" then
         --  Strip quotes from value
         declare
            Val : constant String := S (Node.Children (1).Tag);
         begin
            --  Val should start and end with '"'
            return N ("String", N (Val (Val'First + 1 .. Val'Last - 1)));
         end;
      else
         --  Number or other: return as-is
         return Node;
      end if;
   end Fix_Const;

   ---------------------------------
   -- Parse_Tree_To_OJF_Recurse --
   ---------------------------------

   --  Recurse: the main OJF conversion function.  Transforms a raw parse
   --  tree node into the simplified OJF representation.  Dispatches on the
   --  node's Tag to handle:
   --
   --  Expression / ExpressionClause / ExpressionPhrase / Comparison:
   --    Handles NOT prefix, AND/OR binary operators, and recurses into
   --    sub-expressions.  Builds a tree of logic operators.
   --
   --  PropertyFirstComparison / ConstantFirstComparison:
   --    Extracts the left operand and dispatches on the RHS type:
   --      ValueOpRhs        → simple comparison (=, !=, <, <=, >, >=)
   --      FuzzyStringOpRhs  → CONTAINS / STARTS / ENDS
   --      KnownOpRhs        → IS_KNOWN / IS_UNKNOWN
   --      SetOpRhs          → HAS / HAS_ALL / HAS_ONLY / HAS_ANY / HAS_EXACTLY
   --      SetZipOpRhs       → HAS_ZIP and HAS_ZIP_* variants
   --      LengthOpRhs       → LENGTH comparison
   function Recurse (Node : AST_Node_Access) return AST_Node_Access is
      Tag : constant String := S (Node.Tag);

      --  Working list: tree[0] in Python becomes Result
      Result : AST_Node_Access := null;
      --  Current insertion point
      type Pos_Record is record
         Node : AST_Node_Access;
         Idx  : Natural;  --  Which child index to set (0-based for result)
      end record;
      Pos : Pos_Record;

      procedure Set_Pos_Value (Val : AST_Node_Access) is
      begin
         if Pos.Idx = 0 then
            Result := Val;
         elsif Pos.Idx = 1 then
            Pos.Node.Children.Replace_Element (1, Val);
         elsif Pos.Idx = 2 then
            Pos.Node.Children.Replace_Element (2, Val);
         else
            Pos.Node.Children.Replace_Element (Pos.Idx, Val);
         end if;
      end Set_Pos_Value;

      function Get_Pos_Value return AST_Node_Access is
      begin
         if Pos.Idx = 0 then
            return Result;
         else
            return Pos.Node.Children (Pos.Idx);
         end if;
      end Get_Pos_Value;

   begin
      --  Initialize: tree = [None], pos = tree, arg = 0
      Pos := (Node => null, Idx => 0);

      if Tag = "Expression" or Tag = "ExpressionClause"
        or Tag = "ExpressionPhrase" or Tag = "Comparison"
      then
         --  Check for NOT prefix
         declare
            Start_Idx : Positive := 1;
            Has_Not   : Boolean := False;
         begin
            if Natural (Node.Children.Length) >= 1
              and then S (Node.Children (1).Tag) = "NOT"
            then
               Has_Not := True;
               --  Create NOT node: pos[arg] = ['NOT', None]
               declare
                  Not_Node : constant AST_Node_Access :=
                    N ("NOT", N (""));
               begin
                  Set_Pos_Value (Not_Node);
                  Pos := (Node => Not_Node, Idx => 1);
               end;
               Start_Idx := 2;
            end if;

            --  Process children
            for I in Start_Idx .. Natural (Node.Children.Length) loop
               declare
                  Child     : constant AST_Node_Access := Node.Children (I);
                  Child_Tag : constant String := S (Child.Tag);
               begin
                  if Child_Tag = "Expression"
                    or Child_Tag = "ExpressionClause"
                    or Child_Tag = "ExpressionPhrase"
                    or Child_Tag = "PropertyFirstComparison"
                    or Child_Tag = "ConstantFirstComparison"
                    or Child_Tag = "PredicateComparison"
                    or Child_Tag = "Comparison"
                  then
                     Set_Pos_Value (Recurse (Child));
                  elsif Child_Tag = "AND" or Child_Tag = "OR" then
                     --  Create binary node: pos[arg] = [op, tuple(pos[arg]), None]
                     declare
                        Op_Node : constant AST_Node_Access :=
                          new AST_Node;
                        Prev    : constant AST_Node_Access := Get_Pos_Value;
                     begin
                        Op_Node.Tag := U (Child_Tag);
                        Op_Node.Children.Append (Prev);
                        Op_Node.Children.Append (null);
                        Set_Pos_Value (Op_Node);
                        Pos := (Node => Op_Node, Idx => 2);
                     end;
                  elsif Child_Tag = "OpeningBrace"
                    or Child_Tag = "ClosingBrace"
                  then
                     null;  --  Skip braces
                  else
                     raise Program_Error with
                       "Internal error: filter simplify on invalid ast: "
                       & Child_Tag;
                  end if;
               end;
            end loop;

            if Has_Not then
               null;  --  Result was set through the NOT node
            end if;
         end;

      elsif Tag = "PropertyFirstComparison"
        or Tag = "ConstantFirstComparison"
      then
         --  Extract left operand
         declare
            Left : AST_Node_Access;
         begin
            if Tag = "PropertyFirstComparison" then
               --  left = ('Identifier',) + tuple([x[1] for x in
               --          node[1][1:] if x[0] != 'Dot'])
               Left := new AST_Node;
               Left.Tag := U ("Identifier");
               for Child of Node.Children (1).Children loop
                  if S (Child.Tag) /= "Dot" then
                     Left.Children.Append (N (S (Child.Children (1).Tag)));
                  end if;
               end loop;
            elsif Tag = "ConstantFirstComparison" then
               --  left = _fix_const(node[1][1])
               Left := Fix_Const (Node.Children (1).Children (1));
            end if;

            --  Dispatch on RHS type (node[2][0])
            declare
               RHS     : constant AST_Node_Access := Node.Children (2);
               RHS_Tag : constant String := S (RHS.Tag);
               --  Python len(node[2]) = 1 + Children.Length
               Py_Len  : constant Natural :=
                 1 + Natural (RHS.Children.Length);
            begin
               if RHS_Tag = "ValueOpRhs" then
                  --  (operator, left, right)
                  declare
                     Op    : constant String :=
                       S (RHS.Children (1).Children (1).Tag);
                     Right : constant AST_Node_Access :=
                       Fix_Const (RHS.Children (2).Children (1));
                  begin
                     Result := N (Op, Left, Right);
                  end;

               elsif RHS_Tag = "FuzzyStringOpRhs" then
                  --  (op, left, right) where op is CONTAINS/STARTS/ENDS
                  declare
                     Keyword_Tag : constant String :=
                       S (RHS.Children (1).Tag);
                     Op    : constant String :=
                       S (RHS.Children (1).Children (1).Tag);
                     Right : AST_Node_Access;
                  begin
                     if (Keyword_Tag = "STARTS" or Keyword_Tag = "ENDS")
                       and then Natural (RHS.Children.Length) >= 2
                       and then S (RHS.Children (2).Tag) = "WITH"
                     then
                        Right := RHS.Children (3);
                     else
                        Right := RHS.Children (2);
                     end if;
                     --  Right is Value or Property
                     if S (Right.Tag) = "Value" then
                        Right := Fix_Const (Right.Children (1));
                     end if;
                     Result := N (Op, Left, Right);
                  end;

               elsif RHS_Tag = "KnownOpRhs" then
                  --  (IS_KNOWN or IS_UNKNOWN, operand)
                  declare
                     Op_Str : constant String :=
                       S (RHS.Children (1).Children (1).Tag) & "_"
                       & S (RHS.Children (2).Tag);
                     Operand : AST_Node_Access;
                  begin
                     --  Rebuild operand from Property
                     Operand := new AST_Node;
                     Operand.Tag := U ("Identifier");
                     for Child of Node.Children (1).Children loop
                        if S (Child.Tag) /= "Dot" then
                           Operand.Children.Append
                             (N (S (Child.Children (1).Tag)));
                        end if;
                     end loop;
                     Result := N (Op_Str, Operand);
                  end;

               elsif RHS_Tag = "SetOpRhs" then
                  --  Various HAS forms
                  declare
                     Child2_Tag : constant String :=
                       (if Py_Len >= 3
                        then S (RHS.Children (2).Tag)
                        else "");
                  begin
                     if Child2_Tag = "Operator" then
                        --  HAS Operator Value: len=4
                        declare
                           InOp  : constant String :=
                             S (RHS.Children (2).Children (1).Tag);
                           Right : constant AST_Node_Access :=
                             Fix_Const (RHS.Children (3).Children (1));
                           Ops   : constant AST_Node_Access :=
                             N (InOp);
                           Vals  : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           Vals.Children.Append (Right);
                           Result := N ("HAS", Ops, Left, Vals);
                        end;
                     elsif Py_Len = 3 then
                        --  Simple HAS Value: HAS_ALL with default =
                        declare
                           Right : constant AST_Node_Access :=
                             Fix_Const (RHS.Children (2).Children (1));
                           Ops   : constant AST_Node_Access := N ("=");
                           Vals  : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           Vals.Children.Append (Right);
                           Result := N ("HAS_ALL", Ops, Left, Vals);
                        end;
                     elsif Py_Len = 4 then
                        --  HAS ALL/ONLY/ANY/EXACTLY ValueList
                        declare
                           Qualifier : constant String :=
                             S (RHS.Children (2).Tag);
                           Op        : constant String :=
                             "HAS_" & Qualifier;
                           VList     : constant AST_Node_Access :=
                             RHS.Children (3);
                           InOp      : Unbounded_String :=
                             Null_Unbounded_String;
                           Ops_Node  : constant AST_Node_Access :=
                             new AST_Node;
                           Vals_Node : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           --  Iterate ValueList children
                           for VChild of VList.Children loop
                              declare
                                 VTag : constant String :=
                                   S (VChild.Tag);
                              begin
                                 if VTag = "Operator" then
                                    InOp :=
                                      VChild.Children (1).Tag;
                                 elsif VTag = "Value" then
                                    if Length (InOp) = 0 then
                                       Ops_Node.Children.Append
                                         (N ("="));
                                    else
                                       Ops_Node.Children.Append
                                         (N (S (InOp)));
                                       InOp := Null_Unbounded_String;
                                    end if;
                                    Vals_Node.Children.Append
                                      (Fix_Const (VChild.Children (1)));
                                 end if;
                                 --  Skip Comma and other nodes
                              end;
                           end loop;
                           --  Build result based on ops count
                           declare
                              Final_Ops : AST_Node_Access;
                           begin
                              if Natural (Ops_Node.Children.Length) = 1
                              then
                                 --  Single op: tag = the op value
                                 Final_Ops := Ops_Node.Children (1);
                              else
                                 --  Multiple ops: first is tag,
                                 --  rest are children
                                 Final_Ops := new AST_Node;
                                 Final_Ops.Tag :=
                                   Ops_Node.Children (1).Tag;
                                 for J in 2 ..
                                   Natural (Ops_Node.Children.Length)
                                 loop
                                    Final_Ops.Children.Append
                                      (Ops_Node.Children (J));
                                 end loop;
                              end if;
                              --  Same for values
                              declare
                                 Final_Vals : constant AST_Node_Access :=
                                   new AST_Node;
                              begin
                                 for J in 1 ..
                                   Natural (Vals_Node.Children.Length)
                                 loop
                                    Final_Vals.Children.Append
                                      (Vals_Node.Children (J));
                                 end loop;
                                 Result :=
                                   N (Op, Final_Ops, Left, Final_Vals);
                              end;
                           end;
                        end;
                     else
                        raise Program_Error with
                          "Internal error: unexpected SetOpRhs length:"
                          & Natural'Image (Py_Len);
                     end if;
                  end;

               elsif RHS_Tag = "SetZipOpRhs" then
                  --  PropertyZipAddon handling
                  declare
                     Zip_Addon : constant AST_Node_Access :=
                       RHS.Children (1);
                     --  Build left as tuple of identifiers
                     Left_Zip  : constant AST_Node_Access :=
                       new AST_Node;
                  begin
                     Left_Zip.Children.Append (Left);
                     --  Add identifiers from PropertyZipAddon
                     --  Python: left = (left,) + node[2][1][2::2]
                     --  Skip every other child (Colon, Property pairs)
                     declare
                        Count : Natural := 0;
                     begin
                        for ZChild of Zip_Addon.Children loop
                           Count := Count + 1;
                           if Count mod 2 = 0 then
                              --  This is a Property node
                              Left_Zip.Children.Append (ZChild);
                           end if;
                        end loop;
                     end;

                     if Py_Len = 4 then
                        --  HAS ValueZip
                        declare
                           VZip     : constant AST_Node_Access :=
                             RHS.Children (3);
                           InOp     : Unbounded_String :=
                             Null_Unbounded_String;
                           Ops_Node : constant AST_Node_Access :=
                             new AST_Node;
                           Vals_Node : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           for VChild of VZip.Children loop
                              declare
                                 VTag : constant String :=
                                   S (VChild.Tag);
                              begin
                                 if VTag = "Operator" then
                                    InOp := VChild.Children (1).Tag;
                                 elsif VTag = "Value" then
                                    if Length (InOp) = 0 then
                                       Ops_Node.Children.Append
                                         (N ("="));
                                    else
                                       Ops_Node.Children.Append
                                         (N (S (InOp)));
                                       InOp := Null_Unbounded_String;
                                    end if;
                                    Vals_Node.Children.Append
                                      (Fix_Const (VChild.Children (1)));
                                 end if;
                              end;
                           end loop;
                           declare
                              Final_Ops : AST_Node_Access;
                           begin
                              if Natural (Ops_Node.Children.Length) = 1
                              then
                                 Final_Ops := Ops_Node.Children (1);
                              else
                                 Final_Ops := new AST_Node;
                                 Final_Ops.Tag :=
                                   Ops_Node.Children (1).Tag;
                                 for J in 2 ..
                                   Natural (Ops_Node.Children.Length)
                                 loop
                                    Final_Ops.Children.Append
                                      (Ops_Node.Children (J));
                                 end loop;
                              end if;
                              declare
                                 Final_Vals : constant AST_Node_Access :=
                                   new AST_Node;
                              begin
                                 for J in 1 ..
                                   Natural (Vals_Node.Children.Length)
                                 loop
                                    Final_Vals.Children.Append
                                      (Vals_Node.Children (J));
                                 end loop;
                                 Result := N ("HAS_ZIP", Final_Ops,
                                              Left_Zip, Final_Vals);
                              end;
                           end;
                        end;

                     elsif Py_Len = 5 then
                        --  HAS ALL/ONLY/ANY/EXACTLY ValueZipList
                        declare
                           Qualifier : constant String :=
                             S (RHS.Children (3).Tag);
                           Op        : constant String :=
                             "HAS_ZIP_" & Qualifier;
                           VZList    : constant AST_Node_Access :=
                             RHS.Children (4);
                           All_Ops   : constant AST_Node_Access :=
                             new AST_Node;
                           All_Vals  : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           --  Process ValueZipList: every other child
                           --  is a ValueZip (skip Comma separators)
                           declare
                              Zip_Count : Natural := 0;
                           begin
                              for ZChild of VZList.Children loop
                                 if S (ZChild.Tag) = "ValueZip" then
                                    Zip_Count := Zip_Count + 1;
                                    declare
                                       InOp : Unbounded_String :=
                                         Null_Unbounded_String;
                                       Row_Ops : constant
                                         AST_Node_Access :=
                                           new AST_Node;
                                       Row_Vals : constant
                                         AST_Node_Access :=
                                           new AST_Node;
                                    begin
                                       for VChild of
                                         ZChild.Children
                                       loop
                                          declare
                                             VTag : constant String :=
                                               S (VChild.Tag);
                                          begin
                                             if VTag = "Operator" then
                                                InOp :=
                                                  VChild.Children (1)
                                                    .Tag;
                                             elsif VTag = "Value" then
                                                if Length (InOp) = 0
                                                then
                                                   Row_Ops
                                                     .Children.Append
                                                       (N ("="));
                                                else
                                                   Row_Ops
                                                     .Children.Append
                                                       (N (S (InOp)));
                                                   InOp :=
                                                     Null_Unbounded_String;
                                                end if;
                                                Row_Vals
                                                  .Children.Append
                                                    (Fix_Const
                                                       (VChild
                                                          .Children (1)));
                                             end if;
                                          end;
                                       end loop;
                                       --  Convert row to tuple format
                                       declare
                                          R_Ops : AST_Node_Access;
                                          R_Vals : AST_Node_Access;
                                       begin
                                          if Natural
                                            (Row_Ops.Children.Length)
                                            = 1
                                          then
                                             R_Ops :=
                                               Row_Ops.Children (1);
                                          else
                                             R_Ops := new AST_Node;
                                             R_Ops.Tag :=
                                               Row_Ops.Children (1)
                                                 .Tag;
                                             for K in 2 ..
                                               Natural
                                                 (Row_Ops
                                                    .Children.Length)
                                             loop
                                                R_Ops.Children.Append
                                                  (Row_Ops.Children
                                                     (K));
                                             end loop;
                                          end if;
                                          if Natural
                                            (Row_Vals.Children.Length)
                                            = 1
                                          then
                                             R_Vals :=
                                               Row_Vals.Children (1);
                                          else
                                             R_Vals := new AST_Node;
                                             R_Vals.Tag :=
                                               Row_Vals
                                                 .Children (1).Tag;
                                             for K in 2 ..
                                               Natural
                                                 (Row_Vals
                                                    .Children.Length)
                                             loop
                                                R_Vals.Children.Append
                                                  (Row_Vals
                                                     .Children (K));
                                             end loop;
                                          end if;
                                          All_Ops.Children.Append
                                            (R_Ops);
                                          All_Vals.Children.Append
                                            (R_Vals);
                                       end;
                                    end;
                                 end if;
                              end loop;
                           end;
                           Result := N (Op, All_Ops, Left_Zip, All_Vals);
                        end;
                     else
                        raise Program_Error with
                          "Internal error: unexpected SetZipOpRhs"
                          & " length:"
                          & Natural'Image (Py_Len);
                     end if;
                  end;

               elsif RHS_Tag = "LengthOpRhs" then
                  --  (LENGTH, left, operator, right)
                  declare
                     Op    : Unbounded_String;
                     Right : AST_Node_Access;
                  begin
                     if S (RHS.Children (2).Tag) = "Value" then
                        Op := U ("=");
                        Right :=
                          Fix_Const (RHS.Children (2).Children (1));
                     else
                        Op := RHS.Children (2).Children (1).Tag;
                        Right :=
                          Fix_Const (RHS.Children (3).Children (1));
                     end if;
                     Result :=
                       N ("LENGTH", Left, N (S (Op)), Right);
                  end;

               else
                  raise Program_Error with
                    "Internal error: unrecognized comparison RHS: "
                    & RHS_Tag;
               end if;
            end;
         end;

      else
         raise Program_Error with
           "Internal error: unrecognized node: " & Tag;
      end if;

      return Result;
   end Recurse;

   ----------------------
   -- Parse_Tree_To_OJF --
   ----------------------

   function Parse_Tree_To_OJF
     (AST : AST_Node_Access) return AST_Node_Access
   is
   begin
      --  assert ast[0] == 'Filter'
      if S (AST.Tag) /= "Filter" then
         raise Program_Error with
           "Expected Filter node, got: " & S (AST.Tag);
      end if;
      return Recurse (AST.Children (1));
   end Parse_Tree_To_OJF;

   ---------------
   -- Print_OJF --
   ---------------

   procedure Print_OJF (Node : AST_Node_Access) is

      --  Flat_Repr: produce a single-line string representation of an OJF
      --  node and all its descendants.  Used to test whether a subtree fits
      --  on one line (within 80 columns) before deciding to wrap.
      function Flat_Repr (N : AST_Node_Access) return String is
         Tag : constant String := S (N.Tag);
      begin
         if N.Children.Is_Empty then
            --  Leaf node: 'value'
            return "'" & Tag & "'";
         end if;

         if Tag'Length > 0 then
            --  Tagged tuple: ('tag', child1, child2, ...)
            declare
               Buf : Unbounded_String;
            begin
               Buf := U ("('" & Tag & "'");
               for C of N.Children loop
                  Append (Buf, ", " & Flat_Repr (C));
               end loop;
               Append (Buf, ")");
               return S (Buf);
            end;
         else
            --  Unnamed tuple: (child1, child2, ...)
            declare
               Buf   : Unbounded_String;
               First : Boolean := True;
            begin
               Buf := U ("(");
               for C of N.Children loop
                  if First then
                     First := False;
                  else
                     Append (Buf, ", ");
                  end if;
                  Append (Buf, Flat_Repr (C));
               end loop;
               if Natural (N.Children.Length) = 1 then
                  Append (Buf, ",)");
               else
                  Append (Buf, ")");
               end if;
               return S (Buf);
            end;
         end if;
      end Flat_Repr;

      --  Print_Rec: recursive pretty-printer.  If the flat representation
      --  fits within 80 columns from the current Column position, prints
      --  it on one line.  Otherwise, wraps with one child per line,
      --  indented to Column+1.  Suffix is appended after the closing
      --  paren (e.g. "," or ")" for nested structures).
      procedure Print_Rec
        (Node   : AST_Node_Access;
         Column : Natural;
         Suffix : String)
      is
         Tag  : constant String := S (Node.Tag);
         Flat : constant String := Flat_Repr (Node);
      begin
         if Column + Flat'Length + Suffix'Length <= 80 then
            Put (Flat & Suffix);
            return;
         end if;

         if Node.Children.Is_Empty then
            --  Leaf: always fits, but print anyway
            Put ("'" & Tag & "'" & Suffix);
            return;
         end if;

         declare
            Inner_Col : constant Natural := Column + 1;
            N_Kids    : constant Natural :=
              Natural (Node.Children.Length);
         begin
            if Tag'Length > 0 then
               --  Tagged tuple: ('tag',\n child1,\n ...,\n childN)suffix
               Put ("('" & Tag & "',");
               for I in 1 .. N_Kids loop
                  New_Line;
                  Put ((1 .. Inner_Col => ' '));
                  if I = N_Kids then
                     Print_Rec
                       (Node.Children (I), Inner_Col, ")" & Suffix);
                  else
                     Print_Rec (Node.Children (I), Inner_Col, ",");
                  end if;
               end loop;
            else
               --  Unnamed tuple
               Put ("(");
               --  First child on same line
               if N_Kids = 1 then
                  Print_Rec
                    (Node.Children (1), Inner_Col, ",)" & Suffix);
               else
                  Print_Rec (Node.Children (1), Inner_Col, ",");
                  for I in 2 .. N_Kids loop
                     New_Line;
                     Put ((1 .. Inner_Col => ' '));
                     if I = N_Kids then
                        Print_Rec
                          (Node.Children (I), Inner_Col, ")" & Suffix);
                     else
                        Print_Rec (Node.Children (I), Inner_Col, ",");
                     end if;
                  end loop;
               end if;
            end if;
         end;
      end Print_Rec;

   begin
      Print_Rec (Node, 0, "");
      New_Line;
   end Print_OJF;

end Optimadeparser.Parse;
