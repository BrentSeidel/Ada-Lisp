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
package BBS.lisp.evaluate.misc is
   pragma Elaborate_Body;
   --
   --  Functions for evaluating the various builtin functions.
   --
   --
   --  Exit the lisp interpreter.
   --
   procedure quit(e : out element_type; s : cons_index);
   --
   --  Dump some of the internal tables for debugging purposes.
   --
   procedure dump(e : out element_type; s : cons_index);
   --
   --  Turn debugging messages on or off.
   --
   procedure msg(e : out element_type; s : cons_index);
   --
   --  Pause for the specified number of milliseconds.
   --
   procedure sleep(e : out element_type; s : cons_index);
end;
