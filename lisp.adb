with Ada.Text_IO;
with bbs.lisp;
with new_line;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Lisp is
begin
   Ada.Text_IO.Put_Line("Tiny lisp interpreter written in Ada.");
   bbs.lisp.init(Ada.Text_IO.Put_Line'Access, Ada.Text_IO.Put'Access,
                new_line.New_Line'Access, Ada.Text_IO.Get_Line'Access);
   bbs.lisp.repl;
end Lisp;
