with BBS.lisp.evaluate;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.debug is

   --
   procedure dump(e : element_type) is
   begin
      case e.kind is
         when E_VALUE =>
            dump(e.v);
         when others =>
            Put("Tried to print an unknown element type " & ptr_type'Image(e.kind));
      end case;
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
   procedure dump(v : value) is
   begin
      case v.kind is
         when V_INTEGER =>
            Put(int32'Image(v.i) & " ");
         when V_CHARACTER =>
            Put("'" & v.c & "'");
         when V_STRING =>
            put(" STR: Ref: " & BBS.lisp.strings.str_ref_count'Image(BBS.lisp.strings.ref_count(v.s)) & " Value: ");
            print(v.s);
         when V_BOOLEAN =>
            if v.b then
               put(" T");
            else
               put(" NIL");
            end if;
         when V_LIST =>
            put(" LIST: Ref: " & cons_ref_count'Image(cons_table(v.l).ref) & " Value: ");
            print(v.l);
         when others =>
            Put("<Unknown value kind " & value_type'Image(v.kind) & ">");
      end case;
      Put_Line(">");
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
