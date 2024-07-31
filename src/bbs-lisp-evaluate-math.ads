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
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   pragma Elaborate_Body;
   --
   --  Perform addition
   --
   procedure add(e : out element_type; s : cons_index);
   --
   --  Perform subtraction
   --
   procedure sub(e : out element_type; s : cons_index);
   --
   --  Perform multiplication
   --
   procedure mul(e : out element_type; s : cons_index);
   --
   --  Perform division
   --
   procedure div(e : out element_type; s : cons_index);

private
   --
   --  Helper function for math operations.
   --
   function eval_math(s : cons_index; b : mathops) return element_type;
end;
