with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tokens; use Tokens;

package body Lexer is

   -- Reads the next character and advance the position in the input string
   procedure Read_Char (Lexer : in out Lexer_Type) is
   begin
      if Lexer.Read_Position > Length (Lexer.Input) then
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

   function Next_Token (Lexer : in out Lexer_Type) return Token_Type is
      Token : Token_Type;
   begin
      case Lexer.Ch is
         when '=' =>
            Token := (ASSIGN, 1 * Lexer.Ch);
         when ';' =>
            Token := (SEMICOLON, 1 * Lexer.Ch);
         when '(' =>
            Token := (LPAREN, 1 * Lexer.Ch);
         when ')' =>
            Token := (RPAREN, 1 * Lexer.Ch);
         when ',' =>
            Token := (COMMA, 1 * Lexer.Ch);
         when '+' =>
            Token := (PLUS, 1 * Lexer.Ch);
         when '{' =>
            Token := (LBRACE, 1 * Lexer.Ch);
         when '}' =>
            Token := (RBRACE, 1 * Lexer.Ch);
         when Character'Val (0) =>
            Token := (EOF, To_Unbounded_String (""));
         when others =>
            Token := (ILLEGAL, Null_Unbounded_String);
      end case;
      Read_Char (Lexer);
      return Token;
   end Next_Token;

end Lexer;
