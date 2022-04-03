with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Tokens is

   function Lookup_Identifier (Test : Unbounded_String) return Token_Kind is
      Keywords : array (Positive range <>) of Token_Type :=
        ((K_FUNCTION, To_Unbounded_String ("fn")),
         (K_LET,      To_Unbounded_String ("let")),
         (K_TRUE,     To_Unbounded_String ("true")),
         (K_FALSE,    To_Unbounded_String ("false")),
         (K_IF,       To_Unbounded_String ("if")),
         (K_ELSE,     To_Unbounded_String ("else")),
         (K_RETURN,   To_Unbounded_String ("return")));
      Result : Token_Kind := IDENT;
   begin
      for I in Keywords'Range loop
         if Keywords (I).Literal = Test then
            Result := Keywords (I).Kind;
            exit;
         end if;
      end loop;
      return Result;
   end Lookup_Identifier;

end Tokens;
