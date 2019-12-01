package body bbs.lisp.memory is
   --
   --  Reset the tables
   --
   procedure reset_tables is
   begin
      for i in cons_index loop
         cons_table(i).ref := 0;
      end loop;
      for i in atom_index loop
         atom_table(i).ref := 0;
      end loop;
      for i in symb_index loop
         symb_table(i).ref := 0;
      end loop;
      for i in string_index loop
         string_table(i).ref := 0;
      end loop;
      reset_tempsym;
   end;
   --
   --  Reset the tempsym table.  This is in a separate procedure since it needs
   --  to be done more often.
   --
   procedure reset_tempsym is
   begin
      for i in tempsym_index loop
         tempsym_table(i) := -1;
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
            cons_table(i).car := (Kind => NIL_TYPE);
            cons_table(i).cdr := (Kind => NIL_TYPE);
            return True;
         end if;
      end loop;
      s := 0;
      return False;
   end;
   --
   --  Find an unused atom in the table, mark it as USED, and return the index
   --  in a.  Return false if no such cell could be found.
   --
   function alloc(a : out atom_index) return Boolean is
   begin
      for i in atom_index loop
         if atom_table(i).ref = 0 then
            a := i;
            atom_table(i) := (ref => 1, Kind => ATOM_NIL);
            return True;
         end if;
      end loop;
      a := 0;
      return False;
   end;
   --
   --  Find an unused string fragment, mark it as USED, and return the index
   --  in s.  Return false if no such cell could be found.
   --
   function alloc(s : out string_index) return Boolean is
   begin
      for i in string_index loop
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
   --  Increments the reference count of an atom.
   --
   procedure ref(a : atom_index) is
   begin
      if atom_table(a).ref < Natural'Last then
         atom_table(a).ref := atom_table(a).ref + 1;
      end if;
   end;
   --
   --  Increments the reference count of a cons cell.
   --
   procedure ref(s : cons_index) is
   begin
      cons_table(s).ref :=  cons_table(s).ref + 1;
   end;
   --
   --  Increments the reference count of the item pointed to by an element pointer.
   --
   procedure ref(e : element_type) is
   begin
      if e.kind = CONS_TYPE then
         ref(e.ps);
      elsif e.kind = ATOM_TYPE then
         ref(e.pa);
      end if;
   end;
   --
   --  Decrements the reference count of an atom, checking for locked atoms.
   --
   procedure deref(a : atom_index) is
   begin
      if atom_table(a).ref > 0 then
         if atom_table(a).ref < Natural'Last then
            atom_table(a).ref := atom_table(a).ref - 1;
         end if;
         if atom_table(a).ref = 0 then
            --
            --  If an atom's reference count goes to 0, the if the atom points
            --  to a string, the string's reference count must also go to zero.
            --
            if atom_table(a).kind = ATOM_STRING then
               deref(atom_table(a).str);
            end if;
            atom_table(a) := (ref => 0, Kind => ATOM_NIL);
         end if;
      else
         error("deref atom", "Attempt to deref an unreffed atom at index "
               & Integer'Image(Integer(a)));
      end if;
   end;
   --
   --  Decrements the reference count of a cons cell.
   --
   procedure deref(s : cons_index) is
   begin
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
      if e.kind = CONS_TYPE then
         deref(e.ps);
      elsif e.kind = ATOM_TYPE then
         deref(e.pa);
      end if;
   end;
   --
   --  Decrements the reference count of a string.
   --
   procedure deref(s : string_index) is
      next : Integer;
   begin
      if string_table(s).ref > 0 then
         string_table(s).ref := string_table(s).ref - 1;
      else
         error("deref cons", "Attempt to deref an unreffed string at index "
              & Integer'Image(Integer(s)));
      end if;
      --
      --  If the reference count goes to zero, deref the next fragment.
      --
      if string_table(s).ref = 0 then
         string_table(s).len := 0;
         next := string_table(s).next;
         if (next >= Integer(string_index'First))
           and (next <= Integer(string_index'Last)) then
            deref(string_index(next));
         end if;
         string_table(s).next := -1;
      end if;
   end;
   --
   --  Lock an item so that it can't be dereffed.  This perhaps should not be
   --  used.
   --
   procedure lock(a : atom_index) is
   begin
      if atom_table(a).ref > 0 then
         atom_table(a).ref := Natural'Last;
      else
         error("lock atom", "Cannot lock an unreferenced atom.");
      end if;
   end;
   --
end;
