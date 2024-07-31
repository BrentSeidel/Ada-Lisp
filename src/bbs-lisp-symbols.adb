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
with BBS.lisp.strings;
package body BBS.lisp.symbols is
   --
   --  Operations for symbols
   --
   --  Get the type of the symbol
   --
   function get_type(s : symbol_ptr) return symbol_type is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b.kind;
      else
         return symb_table(s.d).b.kind;
      end if;
   end;
   --
   --  Check if symbol is fixed (builtin or special)
   --
   function isFixed(s : symbol_ptr) return Boolean is
      t : constant symbol_type := get_type(s);
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL) or (s.kind = ST_FIXED);
   end;
   --
   --  Check if symbol is a function (builtin, special, or user defined function)
   --
   function isFunction(s : symbol_ptr) return Boolean is
      t : constant symbol_type := get_type(s);
      e : element_type;
   begin
      if (t = SY_BUILTIN) or (t = SY_SPECIAL) then
         return True;
      end if;
      if t = SY_VARIABLE then
         e := get_value(s);
         return e.kind = V_LAMBDA;
      end if;
      return False;
   end;
   --
   --  If symbol is a variable, get the symbol value.
   --
   function get_value(s : symbol_ptr) return element_type is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b.pv;
      else
         return symb_table(s.d).b.pv;
      end if;
   end;
   --
   --  If symbol is a lambda, get the list.
   --
   function get_list(s : symbol_ptr) return cons_index is
      b : constant sym_body := get_sym(s);
      e : element_type;
   begin
      if b.Kind = SY_VARIABLE then
         e := get_value(s);
         if e.kind = V_LAMBDA then
            return e.lam;
         elsif e.kind = V_LIST then
            return e.l;
         end if;
      end if;
      return NIL_CONS;
   end;
   --
   --  Get a symbol's name
   --
   function get_name(s : symbol_ptr) return string_index is
   begin
      return symb_table(s.d).name;
   end;
   --
   function get_name(s : symbol_ptr) return access constant String is
   begin
      return index(s.f).name;
   end;
   --
   --  Get a symbol's reference count
   --
   function get_ref(s : symbol_ptr) return Natural is
   begin
      if s.kind = ST_FIXED then
         return 2;
      else
         return symb_table(s.d).ref;
      end if;
   end;
   --
   --  Get a symbol from the symbol table
   --
   function get_sym(s : symbol_ptr) return sym_body is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b;
      else
         return symb_table(s.d).b;
      end if;
   end;
   --
   --  Set a symbol entry
   --
   procedure set_sym(s : symbol_ptr; val : sym_body) is
   begin
      symb_table(s.d).b := val;
   end;
   --
   --  Add a new symbol entry
   --
   procedure add_sym(s : symbol_ptr; val : symbol) is
   begin
      symb_table(s.d) := val;
   end;
   --
   --  Reset the symbol table
   --
   procedure reset_symbol_table is
   begin
      for i in symb_table'Range loop
         symb_table(i).ref := 0;
      end loop;
   end;
   --
   --  Search the symbol table for a name.  Assume that the string is already
   --  uppercased.
   --
   function find_name(s : string_index) return symbol_ptr is
   begin
      for i in index'Range loop
         if bbs.lisp.strings.compare(s, index(i).name.all) = CMP_EQ then
            return (kind => ST_FIXED, f => i);
         end if;
      end loop;
      return (kind => ST_NULL);
   end;
   --
end;
