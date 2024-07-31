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
with BBS.lisp.evaluate;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.debug is

   --
   procedure dump(e : element_type) is
   begin
      case e.kind is
         when V_INTEGER =>
            Put(int32'Image(e.i) & " ");
         when V_CHARACTER =>
            Put("'" & e.c & "'");
         when V_STRING =>
            put(" STR: Ref: " & BBS.lisp.strings.str_ref_count'Image(BBS.lisp.strings.ref_count(e.s)) & " Value: ");
            print(e.s);
         when V_BOOLEAN =>
            if e.b then
               put(" T");
            else
               put(" NIL");
            end if;
         when V_LIST =>
            put(" LIST: Ref: " & BBS.lisp.conses.cons_ref_count'Image(BBS.lisp.conses.get_ref(e.l)) & " Value: ");
            print(e.l);
         when others =>
            Put("<Unknown value kind " & value_type'Image(e.kind) & ">");
      end case;
      Put_Line(">");
   end;
   --
   procedure dump(s : cons_index) is
      temp : cons_index := s;
   begin
      Put("(");
      while temp > NIL_CONS loop
         if BBS.lisp.evaluate.isList(BBS.lisp.conses.get_car(temp)) then
            dump(BBS.lisp.evaluate.getList(BBS.lisp.conses.get_car(temp)));
         end if;
         temp := BBS.lisp.evaluate.getList(BBS.lisp.conses.get_cdr(temp));
      end loop;
      put(")");
   end;
   --
   procedure dump(s : symbol_ptr) is
   begin
      if s.kind = ST_FIXED then
         put(BBS.lisp.symbols.get_name(s).all);
      else
         print(BBS.lisp.symbols.get_name(s));
      end if;
      case BBS.lisp.symbols.get_type(s) is
         when SY_BUILTIN =>
            Put(" <BUILTIN>");
         when SY_SPECIAL =>
            Put(" <SPECIAL>");
         when SY_VARIABLE =>
            dump(BBS.lisp.symbols.get_value(s));
         when others =>
            Put(" <UNKNOWN>");
      end case;
   end;
end;
