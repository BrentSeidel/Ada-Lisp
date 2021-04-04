with Ada.Text_IO;
with BBS.lisp;
with BBS.lisp.info;
with new_line;
--
--  This is a simple shell routine to call the embedded lisp interpreter.
--
procedure Lisp is
begin
   Ada.Text_IO.Put_Line("Tiny lisp interpreter written in Ada.");
   Ada.Text_IO.Put_Line(BBS.lisp.info.name & " " & BBs.lisp.info.version_string &
                       " " & BBS.lisp.info.build_date);
   bbs.lisp.init(Ada.Text_IO.Put_Line'Access, Ada.Text_IO.Put'Access,
                new_line.New_Line'Access, Ada.Text_IO.Get_Line'Access);
   bbs.lisp.repl;
end Lisp;
