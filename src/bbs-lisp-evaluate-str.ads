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
--  This package contains functions relating to processing strings.
--
package BBS.lisp.evaluate.str is
   pragma Elaborate_Body;
   --
   --  Return the length of a string or list.  Atoms will get a value of 1.
   --  A NIL pointer returns a length of 0.
   --
   procedure length(e : out element_type; s : cons_index);
   --
   --  Return a specified character from a string.
   --
   procedure char(e : out element_type; s : cons_index);
   --
   --  Parse a string as an integer and return the integer value.
   --
   procedure parse_integer(e : out element_type; s : cons_index);
   --
   --  Return a substring of the original string
   --
   procedure subseq(e : out element_type; s : cons_index);
   --
   --  Convert a string to upper case
   --
   procedure string_upcase(e : out element_type; s : cons_index);
   --
   --  Convert a string to lower case
   --
   procedure string_downcase(e : out element_type; s : cons_index);
private
   --
   --  Helper functions for length of a list (note that length of a string is in
   --  BBS.lisp.strings).
   --
   function length(s : cons_index) return int32;
end;
