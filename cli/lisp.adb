--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of Tiny-Lisp.
--  Tiny-Lisp is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  Tiny-Lisp is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.--
--
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
