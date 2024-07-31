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
package BBS.lisp.evaluate.list is
   pragma Elaborate_Body;
   --
   --  Create a list out of two elements.
   --
   procedure cons(e : out element_type; s : cons_index);
   --
   --  Get the first item of a list
   --
   procedure car(e : out element_type; s : cons_index);
   --
   --  Get the rest of a list
   --
   procedure cdr(e : out element_type; s : cons_index);
   --
   --  Create a list verbatum from the parameter list
   --
   procedure quote(e : out element_type; s : cons_index);
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   procedure list(e : out element_type; s : cons_index);
   --
   --  Replace the CAR of a list with a value
   --
   procedure rplaca(e : out element_type; s : cons_index);
   --
   --  Replace the CDR of a list with a value
   --
   procedure rplacd(e : out element_type; s : cons_index);
   --
   --  Append one list to another (currently unimplemented).
   --
--   procedure append(e : out element_type; s : cons_index);
end;
