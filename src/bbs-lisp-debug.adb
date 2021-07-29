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
            put(" LIST: Ref: " & cons_ref_count'Image(cons_table(e.l).ref) & " Value: ");
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
         if BBS.lisp.evaluate.isList(cons_table(temp).car) then
            dump(BBS.lisp.evaluate.getList(cons_table(temp).car));
         end if;
         temp := BBS.lisp.evaluate.getList(cons_table(temp).cdr);
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
