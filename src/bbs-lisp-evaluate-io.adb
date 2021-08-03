with BBS.lisp.memory;
with BBS.lisp.parser;
with BBS.lisp.parser.string;
with BBS.lisp.strings;
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
      str  : string_index;
      first : string_index;
   begin
      Get_Line(buff, size);
      if BBS.lisp.strings.alloc(str) then
         first := str;
         for ptr in buff'First .. size loop
            if not BBS.lisp.strings.append(str, buff(ptr)) then
               bbs.lisp.strings.deref(str);
               e := NIL_ELEM;
               return;
            end if;
         end loop;
      e := (kind => V_STRING, s => first);
      else
         e := NIL_ELEM;
      end if;
   end;
   --
   --  Read a line from a string and parse it.
   --
   procedure read_expr(e : out element_type; s : cons_index) is
      t   : cons_index := s;
      car : element_type;
   begin
      if s > NIL_CONS then
         car := first_value(t);
         if car.kind /= V_STRING then
            error("read_expr", "Must have a string parameter");
            e := make_error(ERR_WRONGTYPE);
            return;
         end if;
      else
         error("read_expr", "Must have a parameter");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Now we have a string parameter, parse it.
      --
      str.init(car.s);
      if not BBS.lisp.parser.parse(str'Access, e) then
         error("read_expr", "Parsing failed");
         BBS.lisp.memory.deref(e);
         e := make_error(ERR_UNKNOWN);
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
