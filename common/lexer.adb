with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tokens;

package body Lexer is

   -- Reads the next character and advance the position in the input string
   procedure Read_Char (Lexer : in out Lexer_Type) is
   begin
      if Lexer.Read_Position >= Lexer.Input'Size then
         Lexer.Ch := Character'Val (0);
      else
         Lexer.Ch := Element (Lexer.Input, Lexer.Read_Position);
      end if;
      Lexer.Position      := Lexer.Read_Position;
      Lexer.Read_Position := Lexer.Read_Position + 1;
   end Read_Char;

   function New_Lexer (Input : in Unbounded_String) return Lexer_Type is
      Lexer : Lexer_Type :=
        (Input, Positive'First, Positive'First, Character'Val (0));
   begin
      Read_Char (Lexer);
      return Lexer;
   end New_Lexer;

   function Next_Token (Lexer : in out Lexer_Type) return Tokens.Token_Type is
      Token : Tokens.Token_Type := (Tokens.ILLEGAL, Null_Unbounded_String);
   begin
      return Token;
   end Next_Token;

end Lexer;
