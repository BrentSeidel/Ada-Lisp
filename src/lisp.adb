with Ada.Text_IO;
with bbs.lisp;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Lisp is
begin
   Ada.Text_IO.Put_Line("Tiny lisp interpreter written in Ada.");
   bbs.lisp.init;
   bbs.lisp.repl;
end Lisp;
