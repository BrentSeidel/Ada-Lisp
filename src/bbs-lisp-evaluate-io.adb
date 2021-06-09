with BBS.lisp.memory;
with BBS.lisp.parser;
with BBS.lisp.parser.string;
package body BBS.lisp.evaluate.io is
   --
   --  Parser object used for read_expr.  This needs to be statically allocated
   --  so that the address can be taken.
   --
   str : aliased BBS.lisp.parser.string.parser_string;
   --
   --  Print stuff
   --
   procedure print(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      car : element_type;
   begin
      while t > NIL_CONS loop
         car := first_value(t);
         print(car, True, False);
      end loop;
      e := NIL_ELEM;
   end;
   --
   procedure fresh_line(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      if not first_char_flag then
         New_Line;
      end if;
      e := NIL_ELEM;
   end;
   --
   procedure read_line(e : out element_type; s : cons_index) is
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
                  e := NIL_ELEM;
                  return;
               end if;
            end if;
            ptr := ptr + 1;
         end loop;
      end if;
      e := (kind => E_VALUE, v => (kind => V_STRING, s => first));
   end;
   --
   --  Read a line from a string and parse it.
   --
   procedure read_expr(e : out element_type; s : cons_index) is
      t   : cons_index := s;
      car : element_type;
      v   : value;
   begin
      if s > NIL_CONS then
         car := first_value(t);
         if car.kind = E_VALUE then
            v := car.v;
         else
            error("read_expr", "Must have a value parameter");
            e := (kind => E_ERROR);
            return;
         end if;
         if v.kind /= V_STRING then
            error("read_expr", "Must have a string parameter");
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("read_expr", "Must have a parameter");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now we have a string parameter, parse it.
      --
      str.init(v.s);
      if not BBS.lisp.parser.parse(str'Access, e) then
         error("read_expr", "Parsing failed");
         BBS.lisp.memory.deref(e);
         e := (kind => E_ERROR);
      end if;
   end;
   --
   procedure terpri(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      New_Line;
      e := NIL_ELEM;
   end;
   --
end;
