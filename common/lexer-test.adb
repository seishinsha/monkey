with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Tokens;                use Tokens;

package body Lexer.Test is

   procedure Test_Next_Token is
      Input : String                                  := "=+(){},;";
      Tests : array (Positive range <>) of Token_Type :=
        ((ASSIGN, To_Unbounded_String ("=")),
         (PLUS, To_Unbounded_String ("+")),
         (LPAREN, To_Unbounded_String ("(")),
         (RPAREN, To_Unbounded_String (")")),
         (LBRACE, To_Unbounded_String ("{")),
         (RBRACE, To_Unbounded_String ("}")),
         (COMMA, To_Unbounded_String (",")),
         (SEMICOLON, To_Unbounded_String (";")),
         (EOF, To_Unbounded_String ("")));
      Lexer : Lexer_Type := New_Lexer (To_Unbounded_String (Input));
      Token : Token_Type;
   begin

      for I in Tests'Range loop
         Token := Next_Token (Lexer);
         if Token.Kind /= Tests (I).Kind then
            Put_Line
              ("Tests (" & Positive'Image (I) &
               ") - Token kind wrong. Expected=" & Tests (I).Kind'Image &
               ", Got=" & Token.Kind'Image);
         end if;
         if not (Token.Literal = Tests (I).Literal) then
            Put_Line
              ("Tests (" & Positive'Image (I) &
               ") - Token literal wrong. Expected=" &
               To_String (Tests (I).Literal) & ", Got=" &
               To_String (Token.Literal));
         end if;
      end loop;

   end Test_Next_Token;

end Lexer.Test;
