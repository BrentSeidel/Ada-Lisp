with BBS.lisp.memory;
with BBS.lisp.strings;
package body bbs.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer is
      t : element_type := (kind => E_CONS, ps => s);
      c : Integer := 1;
   begin
      while t.kind = E_CONS loop
         c := c + 1;
         t := cons_table(t.ps).cdr;
      end loop;
      if t.kind /= E_NIL then
         c := c + 1;
      end if;
      return c - 1;
   end;
   --
   --  The following routine supports parameters and local variables.
   --  It scans through the passed s expression (recursively, if necessary) and
   --  when it finds a symbol or tempsym, it looks through the list of passed
   --  in parameters or local variables.  If the name matches, it replaces the
   --  symbol or tempsym with a pointer to the parameter or local variable and
   --  updates the ref count.  The return value is the number of replacements
   --  made.
   --
   function replace_syms(s : cons_index; lib : cons_index) return Natural is
      count : Natural := 0;
      temp : cons_index := s;
      new_elem : element_type;

      function process_element(e : element_type; lib : cons_index;
                            replace : out element_type) return Boolean is
         temp : cons_index := lib;
         name : string_index;
         var_elem : element_type;
         var_name : string_index;
         flag : Boolean := False;
      begin
         if e.kind = E_SYMBOL then
            name := symb_table(e.sym).str;
         elsif e.kind = E_TEMPSYM then
            name := e.tempsym;
            flag := True;
         else
            return False;
         end if;
         loop
            var_elem := cons_table(temp).car;
            if var_elem.kind = E_STACK then
               var_name := var_elem.st_name;
            else
               error("replace_syms.process_element", "Improper element in library");
            end if;
            if bbs.lisp.strings.compare(name, var_name) = CMP_EQ then
               if flag then
                  BBS.lisp.memory.deref(name);
               end if;
               replace := var_elem;
               return True;
            end if;
            exit when cons_table(temp).cdr.kind /= E_CONS;
            temp := cons_table(temp).cdr.ps;
         end loop;
         return False;
      end;
      --
   begin
      loop
         if cons_table(temp).car.kind /= E_CONS then
            if process_element(cons_table(temp).car, lib, new_elem) then
               BBS.lisp.memory.deref(cons_table(temp).car);
               cons_table(temp).car := new_elem;
               BBS.lisp.memory.ref(cons_table(temp).car);
               count := count + 1;
            end if;
         elsif cons_table(temp).car.kind = E_CONS then
            count := count + replace_syms(cons_table(temp).car.ps, lib);
         end if;
         exit when cons_table(temp).cdr.kind /= E_CONS;
         temp := cons_table(temp).cdr.ps;
      end loop;
         --
         --  Process the last element, it it exists in a CDR
         --
      if cons_table(temp).cdr.kind /= E_CONS then
         if process_element(cons_table(temp).cdr, lib, new_elem) then
            BBS.lisp.memory.deref(cons_table(temp).cdr);
            cons_table(temp).cdr := new_elem;
            BBS.lisp.memory.ref(cons_table(temp).cdr);
            count := count + 1;
         end if;
      end if;
      return count;
   end;
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
         if e.kind = E_SYMBOL then
            name := symb_table(e.sym).str;
         elsif e.kind = E_TEMPSYM then
            name := e.tempsym;
            flag := True;
         else
            return False;
         end if;
         if var.kind = E_STACK then
            var_name := var.st_name;
         else
            error("replace_syms.process_element", "Improper element in library");
         end if;
         if bbs.lisp.strings.compare(name, var_name) = CMP_EQ then
            if flag then
               BBS.lisp.memory.deref(name);
            end if;
            replace := var;
            return True;
         end if;
         return False;
      end;
      --
   begin
      loop
         if cons_table(temp).car.kind /= E_CONS then
            if process_element(cons_table(temp).car, var, new_elem) then
               BBS.lisp.memory.deref(cons_table(temp).car);
               cons_table(temp).car := new_elem;
               BBS.lisp.memory.ref(cons_table(temp).car);
               count := count + 1;
            end if;
         elsif cons_table(temp).car.kind = E_CONS then
            count := count + replace_sym(cons_table(temp).car.ps, var);
         end if;
         exit when cons_table(temp).cdr.kind /= E_CONS;
         temp := cons_table(temp).cdr.ps;
      end loop;
         --
         --  Process the last element, it it exists in a CDR
         --
      if cons_table(temp).cdr.kind /= E_CONS then
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
end;
