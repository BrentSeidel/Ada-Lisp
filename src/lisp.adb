with Ada.Text_IO;
with bbs.lisp;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Lisp is
   e : BBS.lisp.element_type;
   r : BBS.lisp.element_type;
begin
   Ada.Text_IO.Put_Line("Tiny lisp interpreter written in Ada.");
   bbs.lisp.init;
   while True loop
      e := bbs.lisp.read;
      r := bbs.lisp.eval(e);
      bbs.lisp.print(r, True, True);
      exit when bbs.lisp.exit_lisp;
   end loop;
end Lisp;
