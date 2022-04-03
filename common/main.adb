with Ada.Text_IO; use Ada.Text_IO;
with REPL;

procedure Main is
begin

   Put_Line ("Hello! This is the Monkey programming language!");
   Put_Line ("Feel free to type in commands");
   REPL.Start;

end Main;
