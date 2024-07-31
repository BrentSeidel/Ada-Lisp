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
with BBS.lisp.conses;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.memory is
   --
   --
   --  Reset the tables
   --
   procedure reset_tables is
   begin
      BBS.lisp.conses.reset_cons_table;
      BBS.lisp.symbols.reset_symbol_table;
      BBS.lisp.strings.reset_string_table;
   end;
   --
   --  Increments the reference count of the item pointed to by an element pointer.
   --
   procedure ref(e : element_type) is
   begin
      if e.kind = V_STRING then
         BBS.lisp.strings.ref(e.s);
      elsif e.kind = V_LIST then
         BBS.lisp.conses.ref(e.l);
      elsif e.kind = V_LAMBDA then
         BBS.lisp.conses.ref(e.lam);
      end if;
   end;
   --
   --  Decrements the reference count of the item pointed to by an element pointer.
   --
   procedure deref(e : element_type) is
   begin
      if e.kind = V_STRING then
         BBS.lisp.strings.deref(e.s);
      elsif e.kind = V_LIST then
         BBS.lisp.conses.deref(e.l);
      elsif e.kind = V_LAMBDA then
         BBS.lisp.conses.deref(e.lam);
      elsif e.kind = V_STACK then
         BBS.lisp.strings.deref(e.st_name);
      end if;
   end;
   --
end;
