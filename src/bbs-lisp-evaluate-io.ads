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
--  This package contains the console I/O routines.  They are fairly basic.
--
package BBS.lisp.evaluate.io is
   pragma Elaborate_Body;
   --
   --  Print a list of items.
   --
   procedure print(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
   --
   --  Print an integer in hexidecimal format.  Sizes are byte, word, and long.
   --
   procedure print_hex(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
   --
   --  Print a new line if not already at the beginning of a line.
   --
   procedure fresh_line(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
   --
   --  Read a line from input.
   --
   procedure read_line(e : out element_type; s : cons_index);
   --
   --  Read a line from a string and parse it.
   --
   procedure read_expr(e : out element_type; s : cons_index);
   --
   --  Print a new line.
   --
   procedure terpri(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
end;
