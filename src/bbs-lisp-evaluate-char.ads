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
--  This package contains operations for the character data type.
--
package BBS.lisp.evaluate.char is
   pragma Elaborate_Body;
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   procedure char_code(e : out element_type; s : cons_index);
   --
   --  Given an integer, return the character with that code or an error.
   --
   procedure code_char(e : out element_type; s : cons_index);
   --
   --  If character is alphabetic, convert to upper case.
   --
   procedure char_upcase(e : out element_type; s : cons_index);
   --
   --  If character is alphabetic, convert to lower case.
   --
   procedure char_downcase(e : out element_type; s : cons_index);
end;
