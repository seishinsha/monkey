with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tokens; use Tokens;

package body Lexer is

   function Is_Letter (Ch : in Character) return Boolean is
   begin
      if Ch in 'a' .. 'z' | 'A' .. 'Z' | '_' then
         return True;
      else
         return False;
      end if;
   end Is_Letter;

   function Is_Digit (Ch : in Character) return Boolean is
   begin
      if Ch in '0' .. '9' then
         return True;
      else
         return False;
      end if;
   end Is_Digit;

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

   procedure Skip_Whitespace (Lexer : in out Lexer_Type) is
   begin
      while Lexer.Ch in ' ' | Character'Val (10) | Character'val (13) |
            Character'Val (9)
      loop
         Read_Char (Lexer);
      end loop;
   end Skip_Whitespace;

   function Read_Identifier (Lexer : in out Lexer_Type) return String is
      Initial_Position : Positive := Lexer.Position;
   begin
      loop
         Read_Char (Lexer);
         exit when not Is_Letter (Lexer.Ch);
      end loop;
      return Slice (Lexer.Input, Initial_Position, Lexer.Position - 1);
   end Read_Identifier;

   function Read_Number (Lexer : in out Lexer_Type) return String is
      Initial_Position : Positive := Lexer.Position;
   begin
      loop
         Read_Char (Lexer);
         exit when not Is_Digit (Lexer.Ch);
      end loop;
      return Slice (Lexer.Input, Initial_Position, Lexer.Position - 1);
   end Read_Number;

   function New_Lexer (Input : in Unbounded_String) return Lexer_Type is
      Lexer : Lexer_Type :=
        (Input, Positive'First, Positive'First, Character'Val (0));
   begin
      Read_Char (Lexer);
      return Lexer;
   end New_Lexer;

   function Next_Token (Lexer : in out Lexer_Type) return Token_Type is
      Token : Token_Type := (ILLEGAL, Null_Unbounded_String);
   begin
      Skip_Whitespace (Lexer);
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
            if Is_Letter (Lexer.Ch) then
               Token.Literal := To_Unbounded_String (Read_Identifier (Lexer));
               Token.Kind    := Lookup_Identifier (Token.Literal);
               return Token;
            elsif Is_Digit (Lexer.Ch) then
               Token.Literal := To_Unbounded_String (Read_Number (Lexer));
               Token.Kind    := INT;
               return Token;
            else
               Token.Literal := 1 * Lexer.Ch;
            end if;
      end case;
      Read_Char (Lexer);
      return Token;
   end Next_Token;

end Lexer;
