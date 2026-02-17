with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Miniparser.Lexer is

   --  Position in source text.
   --  Line and Column are 1-based.  Line_Text holds the full text of the
   --  source line (without the trailing newline) for use in error messages.
   type Source_Position is record
      Line      : Natural := 0;
      Column    : Natural := 0;
      Line_Text : Unbounded_String := Null_Unbounded_String;
   end record;

   Null_Position : constant Source_Position := (0, 0, Null_Unbounded_String);

   --  A single token produced by the lexer.
   --
   --  Name holds the token type name (e.g. "Identifier", "Number", or a
   --  literal like "+").  Value holds the matched source text.  Pos records
   --  the source position of the last character of the match.
   --
   --  The final token in every result vector is an end-of-input marker with
   --  Is_End = True and Name/Value set to Null_Unbounded_String.
   type Token_Entry is record
      Name   : Unbounded_String := Null_Unbounded_String;
      Value  : Unbounded_String := Null_Unbounded_String;
      Pos    : Source_Position;
      Is_End : Boolean := False;
   end record;

   --  A token definition: maps a token type name to a regex pattern.
   --  The pattern is matched against accumulated input using GNAT.Regpat
   --  (anchored to the full accumulated string).
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

   --  Comment marker pair for stripping comments during lexing.
   --  Start_Mark begins a comment region, End_Mark ends it.
   --  Markers may span to end-of-line (e.g. Start_Mark="//" End_Mark=<LF>)
   --  but multiline end markers with trailing characters are not supported.
   type Comment_Marker is record
      Start_Mark : Unbounded_String;
      End_Mark   : Unbounded_String;
   end record;
   package Comment_Vectors is new Ada.Containers.Vectors
     (Positive, Comment_Marker);

   --  Raised when the lexer encounters unrecognized input that cannot be
   --  matched to any token, literal, or ignore pattern.
   Parser_Syntax_Error : exception;

   --  Tokenize a source string into a vector of tokens.
   --
   --  The lexer accumulates characters one at a time into a stack, testing
   --  at each step whether the stack matches a literal, a token regex, or a
   --  partial-token regex.  When a full match is found it is recorded; when
   --  the next character breaks all matches, the last full match is emitted
   --  as a token.  This greedy strategy, combined with partial tokens, allows
   --  correct tokenization of constructs like scientific notation (e.g.
   --  "5.32e6" as one Number token rather than Number + Identifier + Number).
   --
   --  Parameters:
   --    Source          — The input string to tokenize.
   --    Tokens          — Ordered list of (name, regex_pattern) pairs defining
   --                      the token types of the language.  Order matters: if
   --                      multiple token patterns match the same input, the
   --                      first match wins.
   --    Partial_Tokens  — Ordered list of (name, regex_pattern) pairs for
   --                      partial token matches.  A partial match means the
   --                      accumulated input could be the prefix of a valid
   --                      token, so the lexer keeps accumulating rather than
   --                      emitting.  Used for greedy matching of multi-part
   --                      tokens (e.g. numbers with exponents).
   --    Literals        — List of literal strings to match exactly.  These are
   --                      tokenized with Name = Value = the literal string.
   --    Ignore          — A string where each character is treated as a
   --                      single-character literal that is matched but not
   --                      emitted (withheld from output).  Commonly used for
   --                      whitespace characters.
   --    Comment_Markers — List of (start, end) marker pairs for comment
   --                      stripping.  Comments are removed during the
   --                      pre-scan phase before tokenization.
   --    Verbosity       — Diagnostic output level.  0 = silent (default).
   --                      4+ = token yields.  5+ = character-level input.
   --
   --  Returns a vector of Token_Entry values, always ending with an
   --  end-of-input marker (Is_End = True).
   --
   --  Raises Parser_Syntax_Error if unrecognized input remains after
   --  processing all characters.
   function Lex
     (Source          : String;
      Tokens          : Token_Def_Vectors.Vector;
      Partial_Tokens  : Token_Def_Vectors.Vector;
      Literals        : UString_Vectors.Vector;
      Ignore          : String;
      Comment_Markers : Comment_Vectors.Vector := Comment_Vectors.Empty_Vector;
      Verbosity       : Natural := 0) return Token_Entry_Vectors.Vector;

end Miniparser.Lexer;
