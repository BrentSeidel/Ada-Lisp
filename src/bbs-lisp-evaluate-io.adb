with BBS.lisp.memory;
package body BBS.lisp.evaluate.io is
   --
   --  Print stuff
   --
   function print(s : cons_index) return element_type is
      t  : cons_index := s;
      car : element_type;
   begin
      while t > NIL_CONS loop
         car := first_value(t);
         print(car, True, False);
      end loop;
      return NIL_ELEM;
   end;
   --
   function fresh_line(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      if not first_char_flag then
         New_Line;
      end if;
      return NIL_ELEM;
   end;
   --
   function read_line(s : cons_index) return element_type is
      pragma Unreferenced (s);
      buff : String(1 .. 256);
      size : Natural;
      ptr : Natural := buff'First;
      str  : string_index;
      next : string_index;
      first : string_index;
      flag : Boolean;
   begin
      Get_Line(buff, size);
      flag := BBS.lisp.memory.alloc(str);
      if flag then
         string_table(str).len := 0;
         string_table(str).next := NIL_STR;
         first := str;
         while (ptr <= size) loop
            if string_table(str).len < fragment_len then
               string_table(str).len := string_table(str).len + 1;
               string_table(str).str(string_table(str).len) := buff(ptr);
            else
               flag := bbs.lisp.memory.alloc(next);
               if flag then
                  string_table(str).next := next;
                  str := next;
                  string_table(str).len := 1;
                  string_table(str).str(1) := buff(ptr);
                  string_table(str).next := NIL_STR;
               else
                  bbs.lisp.memory.deref(first);
                  return NIL_ELEM;
               end if;
            end if;
            ptr := ptr + 1;
         end loop;
      end if;
      ptr := ptr + 1;
      return (kind => E_VALUE, v => (kind => V_STRING, s => first));
   end;
   --
   function terpri(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      New_Line;
      return NIL_ELEM;
   end;
   --
end;
