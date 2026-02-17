with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Miniparser.Lexer;      use Miniparser.Lexer;

package body Miniparser.Parser is

   function U (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   function S (UStr : Unbounded_String) return String
     renames To_String;

   --  State stack
   package Pos_Vectors is new Ada.Containers.Vectors (Positive, Positive);

   --  In_List: check if Name appears in a UString_Vectors list.
   --  Used to test membership in the Simplify, Aggregate, and Remove lists
   --  during the reduce step of the parse loop.
   function In_List
     (V : UString_Vectors.Vector; Name : String) return Boolean
   is
   begin
      for X of V loop
         if S (X) = Name then
            return True;
         end if;
      end loop;
      return False;
   end In_List;

   ------------
   --  Parse --
   ------------

   function Parse
     (LS        : in out Language_Spec_Record;
      Source    : String;
      Verbosity : Natural := 0) return AST_Node_Access
   is
      --  Convert LS fields to Lex parameter types
      Lits_Vec   : UString_Vectors.Vector;
      Ignore_Buf : Unbounded_String;
   begin
      --  Lazy initialization: build parse tables if needed
      if not LS.Has_Parse then
         Build_LS (LS, Verbosity);
      end if;

      --  Literals: String_Sets.Set -> UString_Vectors.Vector
      for L of LS.Literals loop
         Lits_Vec.Append (U (L));
      end loop;

      --  Ignore: String_Sets.Set -> String (concatenate single-char elements)
      for Ign of LS.Ignore loop
         Append (Ignore_Buf, Ign);
      end loop;

      --  Tokenize
      declare
         Token_Vec : constant Token_Entry_Vectors.Vector :=
           Lex (Source, LS.Tokens, LS.Partial_Tokens,
                Lits_Vec, S (Ignore_Buf), LS.Comment_Markers,
                Verbosity);

         --  Stacks
         Symbol_Stack : AST_Vectors.Vector;
         State_Stack  : Pos_Vectors.Vector;

         --  Current token
         Tok_Idx : Positive := 1;
         Symbol  : Unbounded_String;
         Inp     : Unbounded_String;
         Pos     : Source_Position;
      begin
         --  Initialize state stack with start state 1
         State_Stack.Append (1);

         --  Get first token
         Symbol := Token_Vec (Tok_Idx).Name;
         Inp    := Token_Vec (Tok_Idx).Value;
         Pos    := Token_Vec (Tok_Idx).Pos;

         --  Main LR(1) parsing loop.
         --
         --  The loop reads one token at a time and performs the action
         --  indicated by the ACTION table for the current (state, symbol):
         --    Shift  — push the terminal onto the symbol stack, push the
         --             new state onto the state stack, advance to next token.
         --    Reduce — pop RHS_Len symbols from both stacks, filter the
         --             popped nodes (simplify/aggregate/remove), create a
         --             new node tagged with the LHS, push it, and follow
         --             the GOTO table to the next state.
         --    Accept — parsing complete; exit the loop.
         loop
            declare
               Current_State : constant Positive :=
                 State_Stack.Last_Element;
               Sym_Key : constant String := S (Symbol);
            begin
               if Verbosity >= 3 then
                  Ada.Text_IO.Put_Line
                    ("STATE" & Positive'Image (Current_State)
                     & " " & Sym_Key);
               end if;

               --  Look up action
               if not LS.Action_Table.Contains (Current_State)
                 or else not LS.Action_Table (Current_State)
                               .Contains (Sym_Key)
               then
                  if Sym_Key = "" then
                     raise Parser_Syntax_Error with
                       "Parser syntax error: unexpected end of input"
                       & " at line:"
                       & Natural'Image (Pos.Line)
                       & ", pos:" & Natural'Image (Pos.Column)
                       & ":" & ASCII.LF
                       & S (Pos.Line_Text) & ASCII.LF
                       & (1 .. Natural'Max (Pos.Column - 1, 0) => ' ')
                       & "^";
                  else
                     raise Parser_Syntax_Error with
                       "Parser syntax error: unexpected <" & Sym_Key
                       & "> at line:"
                       & Natural'Image (Pos.Line)
                       & ", pos:" & Natural'Image (Pos.Column)
                       & ":" & ASCII.LF
                       & S (Pos.Line_Text) & ASCII.LF
                       & (1 .. Natural'Max (Pos.Column - 1, 0) => ' ')
                       & "^";
                  end if;
               end if;

               declare
                  Act : constant Action_Entry :=
                    LS.Action_Table (Current_State)(Sym_Key);
               begin
                  case Act.Kind is

                  when Shift =>
                     if Verbosity >= 3 then
                        Ada.Text_IO.Put_Line
                          ("PARSE ACTION SHIFT"
                           & Natural'Image (Act.New_State));
                     end if;

                     if Sym_Key = "" then
                        raise Parser_Syntax_Error with
                          "Parser syntax error: unexpected end of"
                          & " input at line:"
                          & Natural'Image (Pos.Line)
                          & ", pos:" & Natural'Image (Pos.Column)
                          & ":" & ASCII.LF
                          & S (Pos.Line_Text) & ASCII.LF
                          & (1 .. Natural'Max (Pos.Column - 1, 0)
                               => ' ')
                          & "^";
                     end if;

                     --  Push terminal node: (symbol, inp)
                     Symbol_Stack.Append (N (Sym_Key, N (S (Inp))));
                     State_Stack.Append (Act.New_State);

                     --  Advance to next token
                     Tok_Idx := Tok_Idx + 1;
                     if Tok_Idx <= Natural (Token_Vec.Length) then
                        Symbol := Token_Vec (Tok_Idx).Name;
                        Inp    := Token_Vec (Tok_Idx).Value;
                        Pos    := Token_Vec (Tok_Idx).Pos;
                     else
                        Symbol := Null_Unbounded_String;
                        Inp    := Null_Unbounded_String;
                     end if;

                  when Reduce =>
                     declare
                        LHS     : constant String :=
                          S (Act.Reduce_LHS);
                        RHS_Len : constant Natural :=
                          Natural (Act.Reduce_RHS.Length);
                        Filtered : AST_Vectors.Vector;
                     begin
                        if Verbosity >= 3 then
                           Ada.Text_IO.Put ("PARSE ACTION REDUCE: "
                                            & LHS & " <-");
                           for Sym of Act.Reduce_RHS loop
                              Ada.Text_IO.Put (" " & S (Sym));
                           end loop;
                           Ada.Text_IO.New_Line;
                        end if;

                        --  Process subnodes with filtering:
                        --    Simplify (or _-prefixed): inline children
                        --    Aggregate: flatten same-type grandchildren
                        --    Remove: discard entirely
                        if RHS_Len > 0 then
                           declare
                              Start_Idx : constant Positive :=
                                Natural (Symbol_Stack.Length)
                                - RHS_Len + 1;
                           begin
                              for I in Start_Idx ..
                                Natural (Symbol_Stack.Length)
                              loop
                                 declare
                                    Node : constant AST_Node_Access :=
                                      Symbol_Stack (I);
                                    Tag  : constant String :=
                                      S (Node.Tag);
                                 begin
                                    if In_List (LS.Simplify, Tag)
                                      or else
                                        (Tag'Length > 0
                                         and then
                                         Tag (Tag'First) = '_')
                                    then
                                       --  Simplify: flatten children
                                       for C of Node.Children loop
                                          Filtered.Append (C);
                                       end loop;
                                    elsif In_List (LS.Aggregate, Tag)
                                    then
                                       --  Aggregate: flatten
                                       --  grandchildren of same type
                                       declare
                                          Collect : AST_Vectors.Vector;
                                       begin
                                          for C of Node.Children loop
                                             if S (C.Tag) = Tag then
                                                for GC of C.Children
                                                loop
                                                   Collect.Append (GC);
                                                end loop;
                                             else
                                                Collect.Append (C);
                                             end if;
                                          end loop;
                                          declare
                                             Agg : constant
                                               AST_Node_Access :=
                                                 new AST_Node;
                                          begin
                                             Agg.Tag := Node.Tag;
                                             Agg.Children := Collect;
                                             Filtered.Append (Agg);
                                          end;
                                       end;
                                    elsif not In_List (LS.Remove, Tag)
                                    then
                                       Filtered.Append (Node);
                                    end if;
                                 end;
                              end loop;
                           end;

                           --  Pop from stacks
                           for I in 1 .. RHS_Len loop
                              Symbol_Stack.Delete_Last;
                              State_Stack.Delete_Last;
                           end loop;
                        end if;
                        --  RHS_Len = 0 (epsilon): stacks unchanged,
                        --  Filtered is empty

                        --  Push new node
                        declare
                           New_Node : constant AST_Node_Access :=
                             new AST_Node;
                        begin
                           New_Node.Tag := U (LHS);
                           New_Node.Children := Filtered;
                           Symbol_Stack.Append (New_Node);
                        end;

                        --  Goto
                        declare
                           Goto_State : constant Positive :=
                             State_Stack.Last_Element;
                        begin
                           if not LS.Goto_Table.Contains (Goto_State)
                             or else not LS.Goto_Table (Goto_State)
                                           .Contains (LHS)
                           then
                              raise Parser_Internal_Error with
                                "Parser internal error: no goto for"
                                & " state"
                                & Positive'Image (Goto_State)
                                & " symbol " & LHS;
                           end if;
                           State_Stack.Append
                             (LS.Goto_Table (Goto_State)(LHS));
                        end;
                     end;

                  when Accept_Action =>
                     exit;

                  end case;
               end;
            end;
         end loop;

         --  Sanity check
         if Natural (Symbol_Stack.Length) > 1 then
            raise Parser_Internal_Error with
              "Parser internal error: unexpected state after"
              & " completed parse:"
              & Natural'Image (Natural (Symbol_Stack.Length))
              & " items on stack";
         end if;

         --  Final simplify of root node
         declare
            Result : AST_Node_Access := Symbol_Stack (1);
         begin
            if In_List (LS.Simplify, S (Result.Tag)) then
               --  Strip the root tag: return node with empty tag
               --  and same children (matches Python's tuple[1:])
               declare
                  Stripped : constant AST_Node_Access :=
                    new AST_Node;
               begin
                  Stripped.Children := Result.Children;
                  Result := Stripped;
               end;
            end if;
            return Result;
         end;
      end;
   end Parse;

end Miniparser.Parser;
