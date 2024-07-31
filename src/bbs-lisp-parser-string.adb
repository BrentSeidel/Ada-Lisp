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
package body BBS.lisp.parser.string is
   --
   --  Initialize the buffer object
   --
   procedure init(self : in out parser_string; s : string_index) is
   begin
      BBS.lisp.strings.init(self.s, s);
   end;
   --
   --  Move pointer to point to the next character in the string
   --
   procedure next_char(self : in out parser_string) is
   begin
      BBS.lisp.strings.next_char(self.s);
   end;
   --
   --  This should advance the pointer until either an end of line character
   --  (ASCII 10 or 13) is reached or the end of the Lisp string is reached.
   --
   procedure next_line(self : in out parser_string) is
      c : Character;
   begin
      c := BBS.lisp.strings.get_char(self.s);
      while self.not_end and (c /= Character'Val(10)) and (c /= Character'Val(13)) loop
         BBS.lisp.strings.next_char(self.s);
         c := BBS.lisp.strings.get_char(self.s);
      end loop;
   end;
   --
   --  Checks if the character after ptr is a digit.
   --
   function is_next_digit(self : parser_string) return Boolean is
   begin
      return isDigit(BBS.lisp.strings.get_next_char(self.s));
   end;
   --
   --  Gets the character selected by ptr.
   --
   function get_char(self : parser_string) return Character is
   begin
      return BBS.lisp.strings.get_char(self.s);
   end;
end;
