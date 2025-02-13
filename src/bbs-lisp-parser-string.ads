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
with BBS.lisp.strings;
package BBS.lisp.parser.string is
   pragma Elaborate_Body;
   --
   --  This object contains the following data:
   --    base    - Pointer to the first fragment of the string (may not be needed)
   --    current - Poiner to the current string fragment
   --    ptr     - Pointer to the current character in the string fragment.
   --
   type parser_string is new parser_buffer with
      record
         s : BBS.lisp.strings.str_iterator;
      end record;
   type parser_string_ptr is access all parser_string'Class;
   --
   --  Gets the character selected by ptr.
   --
   overriding
   function get_char(self : parser_string) return Character;
   --
   --  Checks if the character after ptr is a digit.
   --
   overriding
   function is_next_digit(self : parser_string) return Boolean;
   --
   --  Increment ptr to point to the next character
   --
   overriding
   procedure next_char(self : in out parser_string);
   --
   --  Tests if ptr is less than or equal to last.
   --
   overriding
   function not_end(self : parser_string) return Boolean is (not BBS.lisp.strings.is_end(self.s));
   --
   --  Tests if ptr is greater than last (the opposite of not_end)
   --
   overriding
   function is_end(self : parser_string) return Boolean is (BBS.lisp.strings.is_end(self.s));
   --
   --  This should advance the pointer until either an end of line character
   --  (ASCII 10 or 13) is reached or the end of the Lisp string is reached.
   --
   overriding
   procedure next_line(self : in out parser_string);
   --
   --  Used to request more data.  This will fail when reading a Lisp string.
   --
   overriding
   function request_more(self : in out parser_string) return Boolean is (False);
   --
   --  Check for input end of file.  This is the same as an end of string.
   --
   overriding
   function is_eof(self : in out parser_string) return Boolean is (BBS.lisp.strings.is_end(self.s));
   --
   --  Nothing to do for a string
   --
   overriding
   procedure get_line(self : in out parser_string) is null;
   --
   --  Initializes the object to contain valid values
   --
   procedure init(self : in out parser_string; s : string_index);

end;
