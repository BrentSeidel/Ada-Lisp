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
--  This package contains data and routines for providing data to the parser
--  routines.  This is done to allow the parser to be decoupled from I/O.
--
package bbs.lisp.parser.stdio is
   pragma Elaborate_Body;
   --
   --  This object contains the following data:
   --    buff - The buffer containing the characters
   --    ptr  - Pointer to the current character in the buffer
   --    last - Pointer to the last valid character in the buffer
   --
   type parser_stdio is new parser_buffer with
      record
         buff : String(1 .. 256);
         ptr  : Natural;
         last : Natural;
      end record;
   type parser_stdio_ptr is access all parser_stdio'Class;
   --
   --  Gets the character selected by ptr.
   --
   overriding
   function get_char(self : parser_stdio) return Character is (self.buff(self.ptr));
   --
   --  Checks if the character after ptr is a digit.
   --
   overriding
   function is_next_digit(self : parser_stdio) return Boolean is ((self.ptr <= self.last)
                                                                  and then (isDigit(self.buff(self.ptr + 1))));
   --
   --  Increment ptr to point to the next character
   --
   overriding
   procedure next_char(self : in out parser_stdio);
   --
   --  Tests if ptr is less than or equal to last.
   --
   overriding
   function not_end(self : parser_stdio) return Boolean is (self.ptr <= self.last);
   --
   --  Tests if ptr is greater than last (the opposite of not_end)
   --
   overriding
   function is_end(self : parser_stdio) return Boolean is (self.ptr > self.last);
   --
   --  Since each object only contains a single line, this sets the pointer to
   --  the end of the string so that is_end will return True.
   --
   overriding
   procedure next_line(self : in out parser_stdio);
   --
   --  Reads a line without providing a prompt.  All values in the object are set.
   --
   procedure get_line(self : in out parser_stdio);
   --
   --  Prints a prompt and reads a line.  All values in object are set.
   --
   overriding
   function request_more(self : in out parser_stdio) return Boolean;
   --
   --  Check for input end of file
   --
   overriding
   function is_eof(self : in out parser_stdio) return Boolean;
   --
   --  Initializes the object to contain valid values
   --
   procedure init(self : in out parser_stdio);
private
   eof_char : constant Character := Character'Val(26);  --  Control-Z
end;
