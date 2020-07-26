with BBS.lisp.strings;
with BBS.lisp.memory;
with BBS.lisp.stack;
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
   --  Take an element_type and checks if it can be interpreted as true or false.
   --
   function isTrue(e : element_type) return Boolean is
   begin
      if e.kind = E_NIL then
         return False;
      end if;
      if e.kind = E_VALUE then
         if e.v.kind = V_BOOLEAN  then
            return e.v.b;
         end if;
         return True;
      end if;
      if (cons_table(e.ps).car.kind = E_NIL)
        and (cons_table(e.ps).cdr.kind = E_NIL) then
         return False;
      end if;
      return True;
   end;
   --
   --  A list can be either element type E_CONS or a value of type V_LIST.  This
   --  checks both to see if the element is actually some sort of a list.
   --
   function isList(e : element_type) return Boolean is
   begin
      if e.kind = E_CONS then
         return True;
      end if;
      if e.kind = E_VALUE then
         if e.v.kind = V_LIST then
            return True;
         end if;
      end if;
      return False;
   end;
   --
   --  This checks to see if the element represents a function call.  The element
   --  is a symbol of type either BUILTIN or LAMBDA.
   --
   function isFunction(e : element_type) return Boolean is
      temp : element_type;
   begin
      if e.kind = E_CONS then
         temp := cons_table(e.ps).car;
         if temp.kind = E_SYMBOL then
            if (symb_table(temp.sym).kind = SY_BUILTIN) or
              (symb_table(temp.sym).kind = SY_SPECIAL) or
              (symb_table(temp.sym).kind = SY_LAMBDA) then
               return True;
            end if;
         end if;
      else
         if e.kind = E_SYMBOL then
            if (symb_table(e.sym).kind = SY_BUILTIN) or
              (symb_table(e.sym).kind = SY_SPECIAL) or
              (symb_table(e.sym).kind = SY_LAMBDA) then
               return True;
            end if;
         end if;
      end if;
      return False;
   end;
   --
   --  Note that this only works if e is a list type.
   --
   function getList(e : element_type) return cons_index is
   begin
      if e.kind = E_CONS then
         return e.ps;
      else
         return e.v.l;
      end if;
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
            if var_elem.kind = E_PARAM then
               var_name := var_elem.p_name;
            elsif var_elem.kind = E_LOCAL then
               var_name := var_elem.l_name;
            else
               error("replace_syms.process_atom", "Improper atom in library");
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
         if var.kind = E_PARAM then
            var_name := var.p_name;
         elsif var.kind = E_LOCAL then
            var_name := var.l_name;
         else
            error("replace_syms.process_atom", "Improper atom in library");
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
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, it returns the atom that the variable points to.  If not, it
   --  just returns the atom.  If the variable points to a list, then the
   --  original atom is returned.
   --
   function indirect_elem(e : element_type) return element_type is
      sym : symb_index;
      val : value;
   begin
      if e.kind = E_SYMBOL then
         sym := e.sym;
         if symb_table(sym).kind = SY_VARIABLE then
            return symb_table(sym).pv;
         end if;
      end if;
      if e.kind = E_LOCAL then
         val := BBS.lisp.stack.search_frames(e.l_offset, e.l_name);
         return (kind => E_VALUE, v => val);
      end if;
      if e.kind = E_PARAM then
         val := BBS.lisp.stack.search_frames(e.p_offset, e.p_name);
         return (kind => E_VALUE, v => val);
      end if;
      return e;
   end;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   procedure first_value(e : element_type; car : out element_type; cdr : out element_type) is
      first : element_type;
--      temp : element_type;
      s : cons_index;
   begin
      if e.kind = E_NIL then
         car := NIL_ELEM;
         cdr := NIL_ELEM;
      elsif isList(e) then
         s := e.ps;
         first := cons_table(s).car;
         cdr :=  cons_table(s).cdr;
         if first.kind = E_NIL then
            car := NIL_ELEM;
         elsif isList(first) then
            car := first;
            if isFunction(first) then
               car := eval_dispatch(first.ps);
            else
               BBS.lisp.memory.ref(car);
            end if;
         else
            car := indirect_elem(first);
         end if;
      else
         car := indirect_elem(e);
         cdr := NIL_ELEM;
      end if;
   end;
   --
end;
