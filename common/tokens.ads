with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Tokens is

   type Token_Kind is
     (ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN,
      LBRACE, RBRACE, FUNCT, LET);

   type Token_Type is record
      Kind    : Token_Kind;
      Literal : Unbounded_String;
   end record;

end Tokens;
