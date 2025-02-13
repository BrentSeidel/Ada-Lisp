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
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
package body bbs.lisp.parser.file is
   --
   --  Initializes to an input file
   --
   procedure init(self : in out parser_file; name : String) is
   begin
      if self.valid then
         Ada.Text_IO.Close(self.file);
      end if;
      Ada.Text_IO.Open(self.file, Ada.Text_IO.In_File, name);
      self.valid := True;
      self.ptr := 1;
   exception
      when e : others =>
         Ada.Text_IO.Put_Line("Error opening file: " & name);
         Ada.Text_IO.Put_Line("Exception: " & Ada.Exceptions.Exception_Message(e));
         self.valid := False;
         raise;
   end;
   --
   --  Move pointer to point to the next character in the buffer
   --
   procedure next_char(self : in out parser_file) is
   begin
      self.ptr := self.ptr + 1;
   end;
   --
   --  Sets pointer to the end of the buffer
   --
   procedure next_line(self : in out parser_file) is
   begin
      self.ptr := Ada.Strings.Unbounded.Length(self.buff) + 1;
   end;
   --
   --  Read a line into the buffer
   --
   procedure get_line(self : in out parser_file) is
   begin
      self.buff := Ada.Strings.Unbounded.Text_IO.Get_Line(self.file);
      self.ptr := 1;
   end;
   --
   --  Request more data.  This will always return True since more input
   --  can be read from stdio.  Yes, there are exceptions, but these are on
   --  platforms that can throw exceptions.
   --
   function request_more(self : in out parser_file) return Boolean is
   begin
      self.buff := Ada.Strings.Unbounded.Text_IO.Get_Line(self.file);
      self.ptr := 1;
      return True;
   end;
   --
   --  Check for input end of file
   --
   overriding
   function is_eof(self : in out parser_file) return Boolean is
   begin
      return Ada.Text_IO.End_Of_File(self.file);
   end;
end;
