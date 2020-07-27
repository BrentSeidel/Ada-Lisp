package body bbs.lisp.memory is
   --
   --  Reset the tables
   --
   procedure reset_tables is
   begin
      for i in cons_index loop
         cons_table(i).ref := 0;
      end loop;
      for i in symb_index loop
         symb_table(i).ref := 0;
      end loop;
      for i in string_index'First + 1 .. string_index'Last loop
         string_table(i).ref := 0;
      end loop;
   end;
   --
   --  Find an used cons cell in the table, mark it as USED, and return the
   --  index in s.  Return false if no such cell could be found.
   --
   function alloc(s : out cons_index) return Boolean is
   begin
      for i in cons_index loop
         if cons_table(i).ref = 0 then
            s := i;
            cons_table(i).ref := 1;
            cons_table(i).car := (Kind => E_NIL);
            cons_table(i).cdr := (Kind => E_NIL);
            return True;
         end if;
      end loop;
      s := 0;
      return False;
   end;
   --
   --  Find an unused string fragment, mark it as USED, and return the index
   --  in s.  Return false if no such cell could be found.
   --
   function alloc(s : out string_index) return Boolean is
   begin
      for i in string_index'First + 1 .. string_index'Last loop
         if string_table(i).ref = 0 then
            s := i;
            string_table(i).ref := 1;
            string_table(i).len := 0;
            string_table(i).next := -1;
            return True;
         end if;
      end loop;
      s := 0;
      return False;
   end;
   --
   --  Increments the reference count of a cons cell.
   --
   procedure ref(s : cons_index) is
   begin
      if cons_table(s).ref = 0 then
         error("ref cons", "Attempting to ref an unallocated cons.");
      end if;
      cons_table(s).ref :=  cons_table(s).ref + 1;
   end;
   --
   --  Increments the reference count of a string.
   --
   procedure ref(s : string_index) is
   begin
      if string_table(s).ref = 0 then
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
         end if;
      end if;
   end;
   --
   --  Decrements the reference count of a cons cell.
   --
   procedure deref(s : cons_index) is
   begin
      msg("deref cons", "Dereffing cons at " & Integer'Image(Integer(s)) &
         " Ref count was " & Integer'Image(Integer(cons_table(s).ref)));
      if cons_table(s).ref > 0 then
         cons_table(s).ref := cons_table(s).ref - 1;
      else
         error("deref cons", "Attempt to deref an unreffed cons at index "
              & Integer'Image(Integer(s)));
      end if;
      --
      --  If the reference count goes to zero, deref the things that the cons
      --  points to.
      --
      if cons_table(s).ref = 0 then
         deref(cons_table(s).car);
         deref(cons_table(s).cdr);
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
      end if;
   end;
   --
   --  Decrements the reference count of a string.
   --
   procedure deref(s : string_index) is
      next : string_index;
   begin
      if string_table(s).ref > 0 then
         string_table(s).ref := string_table(s).ref - 1;
      else
         error("deref cons", "Attempt to deref an unreffed string at index "
              & string_index'Image(s));
      end if;
      --
      --  If the reference count goes to zero, deref the next fragment.
      --
      if string_table(s).ref = 0 then
         string_table(s).len := 0;
         next := string_table(s).next;
         if next >= (string_index'First + 1) then
            deref(next);
         end if;
         string_table(s).next := -1;
      end if;
   end;
   --
end;
