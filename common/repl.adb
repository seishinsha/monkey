with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lexer; with Tokens; use Tokens;

package body REPL is

   Prompt : constant String := ">> ";

   procedure Start is
      The_Lexer : Lexer.Lexer_Type;
      The_Token : Tokens.Token_Type;
   begin
      loop
         Put (Prompt);
         The_Lexer := Lexer.New_Lexer (To_Unbounded_String(Get_Line));
         loop
            The_Token := Lexer.Next_Token (The_Lexer);
            exit when The_Token.Kind = Tokens.EOF;
            Put_Line ("{Kind:" & The_Token.Kind'Image & " Literal:" & To_String(The_Token.Literal) & "}");
         end loop;
      end loop;
   end Start;

end REPL;
