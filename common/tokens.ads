with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Tokens is

   type Token_Kind is (
      ILLEGAL,
      EOF,
      
      -- Identifiers and Literals
      IDENT,
      INT,
      
      -- Operators
      ASSIGN,
      PLUS,
      MINUS,
      BANG,
      ASTERISK,
      SLASH,

      LT,
      GT,

      -- Delimiters
      COMMA,
      SEMICOLON,

      LPAREN,
      RPAREN,
      LBRACE,
      RBRACE,

      -- Keywords
      FUNCT,
      LET
      );

   type Token_Type is record
      Kind    : Token_Kind;
      Literal : Unbounded_String;
   end record;

   function Lookup_Identifier (Test : Unbounded_String) return Token_Kind;

end Tokens;
