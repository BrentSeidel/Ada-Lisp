with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.utilities;
with BBS.lisp.stack;
package body BBS.lisp.evaluate is
   --
   function newline(e : element_type) return element_type is
   begin
      New_Line;
      return NIL_ELEM;
   end;
   --
   function reset(e : element_type) return element_type is
   begin
      init;
      return NIL_ELEM;
   end;
   --
   function msg_on(e : element_type) return element_type is
   begin
      msg_flag := True;
      return NIL_ELEM;
   end;
   --
   function msg_off(e : element_type) return element_type is
   begin
      msg_flag := False;
      return NIL_ELEM;
   end;
   --
   function quote(e : element_type) return element_type is
      temp : element_type := e;
   begin
      bbs.lisp.memory.ref(temp);
      return temp;
   end;
   --
   function dump(e : element_type) return element_type is
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      return NIL_ELEM;
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
   function car(e : element_type) return element_type is
      first : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         BBS.lisp.memory.ref(cons_table(s).car);
         BBS.lisp.memory.deref(first);
         return cons_table(s).car;
      end if;
      return first;
   end;
   --
   --  Return the rest of a list
   --
   function cdr(e : element_type) return element_type is
      first : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         BBS.lisp.memory.ref(cons_table(s).cdr);
         BBS.lisp.memory.deref(first);
         return cons_table(s).cdr;
      end if;
      return NIL_ELEM;
   end;
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
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(e : element_type) return element_type is
   begin
      exit_flag := True;
      return NIL_ELEM;
   end;
   --
   function read_line(e : element_type) return element_type is
      buff : String(1 .. 256);
      size : Natural;
      ptr : Natural := buff'First;
      start : Natural := ptr;
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
end;
