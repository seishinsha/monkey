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

      EQ,
      NOT_EQ,
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
      K_FUNCTION,
      K_LET,
      K_TRUE,
      K_FALSE,
      K_IF,
      K_ELSE,
      K_RETURN
      );

   type Token_Type is record
      Kind    : Token_Kind;
      Literal : Unbounded_String;
   end record;

   function Lookup_Identifier (Test : Unbounded_String) return Token_Kind;

end Tokens;
