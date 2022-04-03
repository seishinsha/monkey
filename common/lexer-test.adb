with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Tokens;                use Tokens;

package body Lexer.Test is

   procedure Test_Next_Token is
      Input : String :=
        "let five = 5;"                & ASCII.LF &
        "let ten= 10;"                 & ASCII.LF &
        ""                             & ASCII.LF &
        "let add = fn(x, y) {"         & ASCII.LF &
        "  x + y;"                     & ASCII.LF &
        "};"                           & ASCII.LF &
        ""                             & ASCII.LF &
        "let result = add(five,ten);"  & ASCII.LF &
        "!-/*5;"                       & ASCII.LF &
        "5 < 19 > 5;"                  & ASCII.LF &
        ""                             & ASCII.LF &
        "if (5 < 10) {"                & ASCII.LF &
        "  return true;"               & ASCII.LF &
        "} else {"                     & ASCII.LF &
        "  return false;"              & ASCII.LF &
        "}"                            & ASCII.LF;
      Tests : array (Positive range <>) of Token_Type := 
        ((K_LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("five")),
         (ASSIGN, To_Unbounded_String ("=")),
         (INT, To_Unbounded_String ("5")),
         (SEMICOLON, To_Unbounded_String (";")),
         (K_LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("ten")),
         (ASSIGN, To_Unbounded_String ("=")),
         (INT, To_Unbounded_String ("10")),
         (SEMICOLON, To_Unbounded_String (";")),
         (K_LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("add")),
         (ASSIGN, To_Unbounded_String ("=")),
         (K_FUNCTION, To_Unbounded_String ("fn")),
         (LPAREN, To_Unbounded_String ("(")),
         (IDENT, To_Unbounded_String ("x")),
         (COMMA, To_Unbounded_String (",")),
         (IDENT, To_Unbounded_String ("y")),
         (RPAREN, To_Unbounded_String (")")),
         (LBRACE, To_Unbounded_String ("{")),
         (IDENT, To_Unbounded_String ("x")),
         (PLUS, To_Unbounded_String ("+")),
         (IDENT, To_Unbounded_String ("y")),
         (SEMICOLON, To_Unbounded_String (";")),
         (RBRACE, To_Unbounded_String ("}")),
         (SEMICOLON, To_Unbounded_String (";")),
         (K_LET, To_Unbounded_String ("let")),
         (IDENT, To_Unbounded_String ("result")),
         (ASSIGN, To_Unbounded_String ("=")),
         (IDENT, To_Unbounded_String ("add")),
         (LPAREN, To_Unbounded_String ("(")),
         (IDENT, To_Unbounded_String ("five")),
         (COMMA, To_Unbounded_String (",")),
         (IDENT, To_Unbounded_String ("ten")),
         (RPAREN, To_Unbounded_String (")")),
         (SEMICOLON, To_Unbounded_String (";")),
         (BANG, To_Unbounded_String ("!")),
         (MINUS, To_Unbounded_String ("-")),
         (SLASH, To_Unbounded_String ("/")),
         (ASTERISK, To_Unbounded_String ("*")),
         (INT, To_Unbounded_String ("5")),
         (SEMICOLON, To_Unbounded_String (";")),
         (INT, To_Unbounded_String ("5")),
         (LT, To_Unbounded_String ("<")),
         (INT, To_Unbounded_String ("19")),
         (GT, To_Unbounded_String (">")),
         (INT, To_Unbounded_String ("5")),
         (SEMICOLON, To_Unbounded_String (";")),
         (K_IF, To_Unbounded_String ("if")),
         (LPAREN, To_Unbounded_String ("(")),
         (INT, To_Unbounded_String ("5")),
         (LT, To_Unbounded_String ("<")),
         (INT, To_Unbounded_String ("10")),
         (RPAREN, To_Unbounded_String (")")),
         (LBRACE, To_Unbounded_String ("{")),
         (K_RETURN, To_Unbounded_String ("return")),
         (K_TRUE, To_Unbounded_String ("true")),
         (SEMICOLON, To_Unbounded_String (";")),
         (RBRACE, To_Unbounded_String ("}")),
         (K_ELSE, To_Unbounded_String ("else")),
         (LBRACE, To_Unbounded_String ("{")),
         (K_RETURN, To_Unbounded_String ("return")),
         (K_FALSE, To_Unbounded_String ("false")),
         (SEMICOLON, To_Unbounded_String (";")),
         (RBRACE, To_Unbounded_String ("}")),
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
               ") - Token literal wrong. Expected=""" &
               To_String (Tests (I).Literal) & """, Got=""" &
               To_String (Token.Literal) & """");
         end if;
      end loop;

   end Test_Next_Token;

end Lexer.Test;
