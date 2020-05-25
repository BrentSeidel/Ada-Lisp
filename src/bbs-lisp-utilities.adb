with BBS.lisp.strings;
with BBS.lisp.memory;
package body bbs.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer is
      t : element_type;
      c : Integer := 1;
   begin
      while t.kind = CONS_TYPE loop
         c := c + 1;
         t := cons_table(t.ps).cdr;
      end loop;
      if t.kind = ATOM_TYPE then
         c := c + 1;
      end if;
      return c;
   end;
   --
   --  Take an element_type and checks if it can be interpreted as true or false.
   --
   function is_true(e : element_type) return Boolean is
   begin
      if e.kind = NIL_TYPE then
         return False;
      end if;
      if e.kind = ATOM_TYPE then
         if atom_table(e.pa).kind = ATOM_NIL then
            return False;
         end if;
         return True;
      end if;
      if (cons_table(e.ps).car.kind = NIL_TYPE)
        and (cons_table(e.ps).cdr.kind = NIL_TYPE) then
         return False;
      end if;
      return True;
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
      new_atom : atom_index;

      function process_atom(a : atom_index; lib : cons_index;
                            replace : out atom_index) return Boolean is
         temp : cons_index := lib;
         name : string_index;
         var_atom : atom_index;
         var_name : string_index;
      begin
         if atom_table(a).kind = ATOM_SYMBOL then
            name := symb_table(atom_table(a).sym).str;
         elsif atom_table(a).kind = ATOM_TEMPSYM then
            name := string_index(tempsym_table(atom_table(a).tempsym));
         else
            return False;
         end if;
         loop
            var_atom := cons_table(temp).car.pa;
            if atom_table(var_atom).kind = ATOM_PARAM then
               var_name := atom_table(var_atom).p_name;
            elsif atom_table(var_atom).kind = ATOM_LOCAL then
               var_name := atom_table(var_atom).l_name;
            else
               error("replace_syms.process_atom", "Improper atom in library");
            end if;
            if bbs.lisp.strings.compare(name, var_name) = CMP_EQ then
               replace := var_atom;
               bbs.lisp.memory.ref(replace);
               return True;
            end if;
            exit when cons_table(temp).cdr.kind /= CONS_TYPE;
            temp := cons_table(temp).cdr.ps;
         end loop;
         return False;
      end;
      --
   begin
      loop
         if cons_table(temp).car.kind = ATOM_TYPE then
            if process_atom(cons_table(temp).car.pa, lib, new_atom) then
               bbs.lisp.memory.deref("replace_syms", cons_table(temp).car.pa);
               cons_table(temp).car.pa := new_atom;
               count := count + 1;
            end if;
         elsif cons_table(temp).car.kind = CONS_TYPE then
            count := count + replace_syms(cons_table(temp).car.ps, lib);
         end if;
         exit when cons_table(temp).cdr.kind /= CONS_TYPE;
         temp := cons_table(temp).cdr.ps;
      end loop;
      if cons_table(temp).cdr.kind = ATOM_TYPE then
         if process_atom(cons_table(temp).cdr.pa, lib, new_atom) then
            bbs.lisp.memory.deref("replace_syms", cons_table(temp).cdr.pa);
            cons_table(temp).cdr.pa := new_atom;
            count := count + 1;
         end if;
         --
         --  Process the last atom
         --
      end if;
      return count;
   end;
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, it returns the atom that the variable points to.  If not, it
   --  just returns the atom.  If the variable points to a list, then the
   --  original atom is returned.
   --
   function indirect_atom(a : atom_index) return element_type is
      sym : symb_index;
   begin
      if atom_table(a).kind = ATOM_SYMBOL then
         sym := atom_table(a).sym;
         if symb_table(sym).kind = VARIABLE then
            return symb_table(sym).pv;
         end if;
      end if;
      if atom_table(a).kind = ATOM_LOCAL then
         return atom_table(a).l_value;
      end if;
      if atom_table(a).kind = ATOM_PARAM then
         return atom_table(a).p_value;
      end if;
      return (Kind => ATOM_TYPE, pa => a);
   end;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   procedure first_value(e : element_type; car : out element_type; cdr : out element_type) is
      first : element_type;
      s : cons_index;
   begin
      if e.kind = NIL_TYPE then
         car := NIL_ELEM;
         cdr := NIL_ELEM;
      elsif e.kind = ATOM_TYPE then
         car := indirect_atom(e.pa);
         BBS.lisp.memory.ref(car);
         cdr := NIL_ELEM;
      else -- The only other option is CONS_TYPE
         s := e.ps;
         first := cons_table(s).car;
         if first.kind = NIL_TYPE then
            car := NIL_ELEM;
         elsif first.kind = ATOM_TYPE then
            car := indirect_atom(first.pa);
            BBS.lisp.memory.ref(car);
         else -- The first item is a CONS_TYPE
            car := eval_dispatch(first.ps);
         end if;
         cdr := cons_table(s).cdr;
      end if;
   end;
   --
end;
