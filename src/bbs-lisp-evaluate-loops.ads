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
--  This package contains the functions for Lisp loop operations.
--
package BBS.lisp.evaluate.loops is
   --
   --  Evaluate statements while a condition is true.
   --
   procedure dowhile(e : out element_type; s : cons_index);
   --
   --  Evaluate statements a specified number of times.
   --
   procedure dotimes(e : out element_type; s : cons_index; p : phase);
   --
   --  Evaluate statements for elements in a list.
   --
   procedure dolist(e : out element_type; s : cons_index; p : phase);
   --
   --  Create a block containing multiple statements
   --
   procedure progn(e : out element_type; s : cons_index);
   --
   --  Breaks out of a loop (or other exclosing block) and returns a value
   --
   procedure return_from(e : out element_type; s : cons_index);
end;
