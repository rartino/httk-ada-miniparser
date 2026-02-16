with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Miniparser.Lexer is

   --  Position in source text
   type Source_Position is record
      Line      : Natural := 0;
      Column    : Natural := 0;
      Line_Text : Unbounded_String := Null_Unbounded_String;
   end record;

   Null_Position : constant Source_Position := (0, 0, Null_Unbounded_String);

   --  A single token produced by the lexer.
   --  For the end-of-input marker, Is_End = True and Name/Value are null.
   type Token_Entry is record
      Name   : Unbounded_String := Null_Unbounded_String;
      Value  : Unbounded_String := Null_Unbounded_String;
      Pos    : Source_Position;
      Is_End : Boolean := False;
   end record;

   --  A token definition: maps a name to a regex pattern
   type Token_Def is record
      Name    : Unbounded_String;
      Pattern : Unbounded_String;
   end record;

   package Token_Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Token_Entry);

   package Token_Def_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Token_Def);

   package UString_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Unbounded_String);

   --  Comment markers for stripping comments during lexing
   type Comment_Marker is record
      Start_Mark : Unbounded_String;
      End_Mark   : Unbounded_String;
   end record;
   package Comment_Vectors is new Ada.Containers.Vectors
     (Positive, Comment_Marker);

   Parser_Syntax_Error : exception;

   --  Tokenize source into a vector of tokens.
   --
   --  Tokens: ordered list of (name, regex_pattern) for token types.
   --  Partial_Tokens: ordered list of (name, regex_pattern) for partial
   --    matches that allow greedy matching (e.g., scientific notation).
   --  Literals: list of literal strings to match exactly.
   --  Ignore: a string where each character is a pattern to match and discard.
   --  Comment_Markers: list of (start, end) pairs for comment stripping.
   --  Verbosity: diagnostic output level (0 = silent).
   function Lex
     (Source          : String;
      Tokens          : Token_Def_Vectors.Vector;
      Partial_Tokens  : Token_Def_Vectors.Vector;
      Literals        : UString_Vectors.Vector;
      Ignore          : String;
      Comment_Markers : Comment_Vectors.Vector := Comment_Vectors.Empty_Vector;
      Verbosity       : Natural := 0) return Token_Entry_Vectors.Vector;

end Miniparser.Lexer;
