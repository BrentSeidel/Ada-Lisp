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
package BBS.lisp.evaluate.symb is
   pragma Elaborate_Body;
   --
   --  Coerces an object of one type to another type.  Available coercions are:
   --    character -> string
   --    boolean -> string
   --    boolean -> integer (NIL -> 0, T -> 1)
   --    integer -> boolean (0 -> NIL, /= 0 -> T)
   --
   procedure coerce(e : out element_type; s : cons_index);
   --
   --  Concatenate two strings or lists.
   --
   procedure concatenate(e : out element_type; s : cons_index);

private
   --
   --  The first time one of these functions is called, populate the symbol
   --  indices so the symbol table doesn't have to be searched each time.
   --
   not_initialized : Boolean := True;
   sym_bool : symbol_ptr := (kind => ST_NULL);
   sym_char : symbol_ptr := (kind => ST_NULL);
   sym_int  : symbol_ptr := (kind => ST_NULL);
   sym_list : symbol_ptr := (kind => ST_NULL);
   sym_str  : symbol_ptr := (kind => ST_NULL);
   --
   --  Initialize the symbol indices.  Return true if successful or false it not.
   --
   function init_syms return Boolean;
end;
