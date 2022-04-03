with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Tokens;                use Tokens;

package body Lexer.Test is

   procedure Test_Next_Token is
      Input : String :=
        "let five = 5;" & Character'Val (10) & "let ten= 10;" &
        Character'Val (10) & Character'Val (10) & "let add = fn(x, y) {" &
        Character'Val (10) & "  x + y;" & Character'Val (10) & "};" &
        Character'Val (10) & Character'Val (10) &
        "let result = add(five,ten);";
      Tests : array (Positive range <>) of Token_Type :=
        ((LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("five")),
         (ASSIGN, To_Unbounded_String ("=")), (INT, To_Unbounded_String ("5")),
         (SEMICOLON, To_Unbounded_String (";")),
         (LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("ten")),
         (ASSIGN, To_Unbounded_String ("=")),
         (INT, To_Unbounded_String ("10")),
         (SEMICOLON, To_Unbounded_String (";")),
         (LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("add")),
         (ASSIGN, To_Unbounded_String ("=")),
         (FUNCT, To_Unbounded_String ("fn")),
         (LPAREN, To_Unbounded_String ("(")),
         (IDENT, To_Unbounded_String ("x")),
         (COMMA, To_Unbounded_String (",")),
         (IDENT, To_Unbounded_String ("y")),
         (RPAREN, To_Unbounded_String (")")),
         (LBRACE, To_Unbounded_String ("{")),
         (IDENT, To_Unbounded_String ("x")), (PLUS, To_Unbounded_String ("+")),
         (IDENT, To_Unbounded_String ("y")),
         (SEMICOLON, To_Unbounded_String (";")),
         (RBRACE, To_Unbounded_String ("}")),
         (LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("result")),
         (ASSIGN, To_Unbounded_String ("=")),
         (IDENT, To_Unbounded_String ("add")),
         (LPAREN, To_Unbounded_String ("(")),
         (IDENT, To_Unbounded_String ("five")),
         (COMMA, To_Unbounded_String (",")),
         (IDENT, To_Unbounded_String ("ten")),
         (RPAREN, To_Unbounded_String (")")),
         (RPAREN, To_Unbounded_String (")")),
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
