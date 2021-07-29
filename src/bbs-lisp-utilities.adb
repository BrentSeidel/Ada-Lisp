with BBS.lisp.evaluate;
with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer is
      t : cons_index := s;
      last : cons_index;
      c : Integer := 0;
   begin
      if s = NIL_CONS then
         return 0;
      end if;
      while t > NIL_CONS loop
         c := c + 1;
         last := t;
         t := BBS.lisp.evaluate.getList(cons_table(t).cdr);
      end loop;
      if cons_table(last).cdr /= NIL_ELEM then
         c := c + 1;
      end if;
      return c;
   end;
   --
   --  The following routine supports parameters and local variables.
   --  It scans through the passed s expression (recursively, if necessary) and
   --  when it finds a symbol, it looks at the passed in parameter or local
   --  variable.  If the name matches, it replaces the symbol with a pointer to
   --  the parameter or local variable and updates the ref count.  The return
   --  value is the number of replacements made.
   --
   --  Perform the replacement for a single symbol/variable.  Searches the list
   --  s and any symbols or tempsyms whose name matches that of var are replaced
   --  by var.  This means that stack variables will shadow symbols.
   --
   function replace_sym(s : cons_index; var : element_type) return Natural is
      count : Natural := 0;
      temp : cons_index := s;
      new_elem : element_type;

      function process_element(e : element_type; var : element_type;
                            replace : out element_type) return Boolean is
         name : string_index;      --  Name of item to potentially replace
         var_name : string_index;  --  Name of potential replacement
         flag : Boolean := False;  --  Was it a tempsym?
      begin
         if e.kind = V_SYMBOL then
            if e.sym.kind = ST_FIXED then
               return False;
            end if;
            name := BBS.lisp.symbols.get_name(e.sym);
         elsif e.kind = V_TEMPSYM then
            name := e.tempsym;
            flag := True;
         else
            return False;
         end if;
         if var.kind = V_STACK then
            var_name := var.st_name;
         else
            error("replace_syms.process_element", "Improper element in library");
         end if;
         if bbs.lisp.strings.compare(name, var_name) = CMP_EQ then
            if flag then
               BBS.lisp.strings.deref(name);
            end if;
            replace := var;
            return True;
         end if;
         return False;
      end;
      --
   begin
      loop
         if BBS.lisp.evaluate.isList(cons_table(temp).car) then
            count := count + replace_sym(BBS.lisp.evaluate.getList(cons_table(temp).car), var);
         else
            if process_element(cons_table(temp).car, var, new_elem) then
               BBS.lisp.memory.deref(cons_table(temp).car);
               cons_table(temp).car := new_elem;
               BBS.lisp.memory.ref(cons_table(temp).car);
               count := count + 1;
            end if;
         end if;
         exit when not BBS.lisp.evaluate.isList(cons_table(temp).cdr);
         temp := BBS.lisp.evaluate.getList(cons_table(temp).cdr);
      end loop;
         --
         --  Process the last element, it it exists in a CDR
         --
      if not BBS.lisp.evaluate.isList(cons_table(temp).cdr) then
         if process_element(cons_table(temp).cdr, var, new_elem) then
            BBS.lisp.memory.deref(cons_table(temp).cdr);
            cons_table(temp).cdr := new_elem;
            BBS.lisp.memory.ref(cons_table(temp).cdr);
            count := count + 1;
         end if;
      end if;
      return count;
   end;
   --
   --  Is character a decimal digit?
   --
   function isDigit(c : Character) return Boolean is
   begin
      return (c >= '0' and c <= '9');
   end;
   --
   --  Is character an alphabetic character
   --
   function isAlpha(c : Character) return Boolean is
   begin
      return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
   end;
   --
   -- Is character a hexidecimal digit?
   --
   function isHex(c : Character) return Boolean is
   begin
      return (c >= '0' and c <= '9') or (c >= 'A' and c <= 'F')
        or (c >= 'a' and c <= 'f');
   end;
   --
   function hexDigit(c : Character) return uint32 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
end;
