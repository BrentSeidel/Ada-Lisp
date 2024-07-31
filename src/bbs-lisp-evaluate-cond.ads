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
--  This package contains the Lisp comparison and condition functions
--
package BBS.lisp.evaluate.cond is
   pragma Elaborate_Body;
   --
   --  Helper function for comparisons
   --
   function eval_comp(s : cons_index; b : compops) return element_type;
   --
   --  Compare two items for equality.
   --
   procedure eq(e : out element_type; s : cons_index);
   --
   --  Compare two items for not equality.
   --
   procedure ne(e : out element_type; s : cons_index);
   --
   --  Is first item less than the second item?
   --
   procedure lt(e : out element_type; s : cons_index);
   --
   --  Is the first item greater than the second item?
   --
   procedure gt(e : out element_type; s : cons_index);
   --
   --  Perform an IF operation.
   --
   procedure eval_if(e : out element_type; s : cons_index);
   --
   --  Perform a COND operation.
   --
   procedure eval_cond(e : out element_type; s : cons_index);
end;
