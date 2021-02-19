package body bbs.lisp.memory is
   --
   --  Ghost functions used in some proofs.
   --
   function count_free_cons return Natural is
      count : Natural := 0;
   begin
      for i in cons_table'Range loop
         if cons_table(i).ref = 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end;
   --
   function count_free_str return Natural is
      count : Natural := 0;
   begin
      for i in string_table'Range loop
         if string_table(i).ref = 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end;
   --
   --  Reset the tables
   --
   procedure reset_tables is
   begin
      for i in cons_table'Range loop
         cons_table(i).ref := FREE_CONS;
         cons_table(i).car := (Kind => E_NIL);
         cons_table(i).cdr := (Kind => E_NIL);
      end loop;
      for i in symb_table'Range loop
         symb_table(i).ref := 0;
      end loop;
      for i in string_table'Range loop
         string_table(i).ref := FREE_STR;
      end loop;
   end;
   --
   --  Find an used cons cell in the table, mark it as USED, and return the
   --  index in s.  Return false if no such cell could be found.
   --
   function alloc(s : out cons_index) return Boolean is
   begin
      for i in cons_table'Range loop
         if cons_table(i).ref = FREE_CONS then
            s := i;
            cons_table(i).ref := FREE_CONS + 1;
            cons_table(i).car := (Kind => E_NIL);
            cons_table(i).cdr := (Kind => E_NIL);
            return True;
         end if;
      end loop;
      s := NIL_CONS;
      return False;
   end;
   --
   --  Find an unused string fragment, mark it as USED, and return the index
   --  in s.  Return false if no such cell could be found.
   --
   function alloc(s : out string_index) return Boolean is
   begin
      for i in string_table'Range loop
         if string_table(i).ref = FREE_STR then
            s := i;
            string_table(i).ref := FREE_STR + 1;
            string_table(i).len := 0;
            string_table(i).next := NIL_STR;
            return True;
         end if;
      end loop;
      s := NIL_STR;
      return False;
   end;
   --
   --  Increments the reference count of a cons cell.
   --
   procedure ref(s : cons_index) is
   begin
      if cons_table(s).ref = FREE_CONS then
         error("ref cons", "Attempting to ref an unallocated cons.");
      end if;
      cons_table(s).ref :=  cons_table(s).ref + 1;
   end;
   --
   --  Increments the reference count of a string.
   --
   procedure ref(s : string_index) is
   begin
      if string_table(s).ref = FREE_STR then
         error("ref string", "Attempting to ref an unallocated string.");
      end if;
    string_table(s).ref := string_table(s).ref + 1;
   end;
   --
   --  Increments the reference count of the item pointed to by an element pointer.
   --
   procedure ref(e : element_type) is
   begin
      if e.kind = E_CONS then
         ref(e.ps);
      elsif e.kind = E_VALUE then
         if e.v.kind = V_STRING then
            ref(e.v.s);
         elsif e.v.kind = V_LIST then
            ref(e.v.l);
         elsif e.v.kind = V_LAMBDA then
            ref(e.v.lam);
         end if;
      end if;
   end;
   --
   --  Increments the reference count of a value, if applicable.
   --
   procedure ref(v : value) is
   begin
      if v.kind = V_STRING then
         ref(v.s);
      elsif v.kind = V_LIST then
         ref(v.l);
      elsif v.kind = V_LAMBDA then
         ref(v.lam);
      end if;
   end;
   --
   --  Decrements the reference count of a cons cell.
   --
   procedure deref(s : cons_index) is
   begin
      if s > NIL_CONS then
         msg("deref cons", "Dereffing cons at " & cons_index'Image(s) &
            " Ref count was " & Integer'Image(Integer(cons_table(s).ref)));
         if cons_table(s).ref > FREE_CONS then
            cons_table(s).ref := cons_table(s).ref - 1;
         else
            error("deref cons", "Attempt to deref an unreffed cons at index "
               & cons_index'Image(s));
         end if;
         --
         --  If the reference count goes to zero, deref the things that the cons
         --  points to.
         --
         if cons_table(s).ref = FREE_CONS then
            deref(cons_table(s).car);
            deref(cons_table(s).cdr);
            cons_table(s).car := NIL_ELEM;
            cons_table(s).cdr := NIL_ELEM;
         end if;
      end if;
   end;
   --
   --  Decrements the reference count of the item pointed to by an element pointer.
   --
   procedure deref(e : element_type) is
   begin
      if e.kind = E_CONS then
         deref(e.ps);
      elsif e.kind = E_VALUE then
         deref(e.v);
      elsif e.kind = E_STACK then
         deref(e.st_name);
      end if;
   end;
   --
   --  Decrements the reference count of the item pointed to by a value.
   --
   procedure deref(v : value) is
   begin
      if v.kind = V_STRING then
         deref(v.s);
      elsif v.kind = V_LIST then
         deref(v.l);
      elsif v.kind = V_LAMBDA then
         deref(v.lam);
      end if;
   end;
   --
   --  Decrements the reference count of a string.
   --
   procedure deref(s : string_index) is
      next : string_index;
      prev : string_index;
   begin
      if string_table(s).ref > FREE_STR then
         string_table(s).ref := string_table(s).ref - 1;
      else
         error("deref string", "Attempt to deref an unreffed string at index "
              & string_index'Image(s));
      end if;
      --
      --  If the reference count goes to zero, deref the next fragment.
      --
      if string_table(s).ref = FREE_STR then
         prev := s;
         next := string_table(s).next;
         string_table(prev).len := 0;
         string_table(prev).next := NIL_STR;
         while next > NIL_STR loop
            if string_table(next).ref > FREE_STR then
               string_table(next).ref := string_table(next).ref - 1;
            else
               error("deref string", "Attempt to deref an unreffed string at index "
                     & string_index'Image(s));
            end if;
            exit when string_table(next).ref > FREE_STR;
            prev := next;
            next := string_table(prev).next;
            string_table(prev).len := 0;
            string_table(prev).next := NIL_STR;
         end loop;
      end if;
   end;
   --
end;
