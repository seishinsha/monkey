with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tokens;

package Lexer is

   type Lexer_Type is record
      Input         : Unbounded_String;
      Position : Positive;   -- Current position in input (points to current char)
      Read_Position : Positive;   -- Current reading position in input (after current char)
      Ch            : Character; -- Current char under examination
   end record;

   function New_Lexer (Input : in Unbounded_String) return Lexer_Type;

   function Next_Token (Lexer : in out Lexer_Type) return Tokens.Token_Type;

end Lexer;
