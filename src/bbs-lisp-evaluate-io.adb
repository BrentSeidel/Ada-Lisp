with BBS.lisp.memory;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.io is
   --
   --  Print stuff
   --
   function print(e : element_type) return element_type is
      t : element_type := e;
      result : element_type;
      car : element_type;
   begin
      while t.kind = E_CONS loop
         car := cons_table(t.ps).car;
         if car.kind /= E_CONS then
            result := bbs.lisp.utilities.indirect_elem(car);
            print(result, False, False);
         elsif car.kind = E_CONS then
            result := eval_dispatch(car.ps);
            print(result, True, False);
         end if;
         t := cons_table(t.ps).cdr;
      end loop;
      if t.kind /= E_NIL then
         print(t, False, True);
      end if;
      return NIL_ELEM;
   end;
   --
   function fresh_line(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      if not first_char_flag then
         New_Line;
      end if;
      return NIL_ELEM;
   end;
   --
   function read_line(e : element_type) return element_type is
      pragma Unreferenced (e);
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
         string_table(str).next := -1;
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
                  string_table(str).next := -1;
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
   function terpri(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      New_Line;
      return NIL_ELEM;
   end;
   --
end;