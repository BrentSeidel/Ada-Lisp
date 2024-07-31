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
package body bbs.lisp.parser.stdio is
   --
   --  Initialize the buffer object
   --
   procedure init(self : in out parser_stdio) is
   begin
      self.ptr := self.buff'First;
      self.last := self.buff'First;
   end;
   --
   --  Move pointer to point to the next character in the buffer
   --
   procedure next_char(self : in out parser_stdio) is
   begin
      self.ptr := self.ptr + 1;
   end;
   --
   --  Sets pointer to the end of the buffer
   --
   procedure next_line(self : in out parser_stdio) is
   begin
      self.ptr := self.last + 1;
   end;
   --
   --  Read a line into the buffer
   --
   procedure get_line(self : in out parser_stdio) is
   begin
      Get_Line(self.buff, self.last);
      self.ptr := self.buff'First;
   end;
   --
   --  Request more data.  This will always return True since more input
   --  can be read from stdio.  Yes, there are exceptions, but these are on
   --  platforms that can throw exceptions.
   --
   function request_more(self : in out parser_stdio) return Boolean is
   begin
      Put(prompt2);
      Get_Line(self.buff, self.last);
      self.ptr := self.buff'First;
      return True;
   end;
end;
