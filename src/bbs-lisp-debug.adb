with BBS.lisp.evaluate;
with BBS.lisp.strings;
package body bbs.lisp.debug is

   --
   procedure dump(e : element_type) is
   begin
      case e.kind is
         when E_NIL =>
            put(" NIL");
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
            put(" STR: Ref: " & str_ref_count'Image(BBS.lisp.strings.ref_count(v.s)) & " Value: ");
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
   procedure dump(s : symb_index) is
   begin
      print(symb_table(s).str);
      case symb_table(s).kind is
         when SY_BUILTIN =>
            Put(" <BUILTIN>");
         when SY_SPECIAL =>
            Put(" <SPECIAL>");
         when SY_LAMBDA =>
            Put(" <FUNCTION>");
         when SY_VARIABLE =>
            dump(symb_table(s).pv);
         when others =>
            Put(" <UNKNOWN>");
      end case;
   end;
end;
