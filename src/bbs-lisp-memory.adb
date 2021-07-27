with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.memory is
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
   --
   --  Reset the tables
   --
   procedure reset_tables is
   begin
      for i in cons_table'Range loop
         cons_table(i).ref := FREE_CONS;
         cons_table(i).car := NIL_ELEM;
         cons_table(i).cdr := NIL_ELEM;
      end loop;
      BBS.lisp.symbols.reset_symbol_table;
      BBS.lisp.strings.reset_string_table;
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
            cons_table(i).car := NIL_ELEM;
            cons_table(i).cdr := NIL_ELEM;
            return True;
         end if;
      end loop;
      s := NIL_CONS;
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
   --  Increments the reference count of the item pointed to by an element pointer.
   --
   procedure ref(e : element_type) is
   begin
      if e.kind = E_VALUE then
         if e.v.kind = V_STRING then
            BBS.lisp.strings.ref(e.v.s);
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
         BBS.lisp.strings.ref(v.s);
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
      if e.kind = E_VALUE then
         deref(e.v);
--      elsif e.kind = E_STACK then
--         BBS.lisp.strings.deref(e.st_name);
      end if;
   end;
   --
   --  Decrements the reference count of the item pointed to by a value.
   --
   procedure deref(v : value) is
   begin
      if v.kind = V_STRING then
         BBS.lisp.strings.deref(v.s);
      elsif v.kind = V_LIST then
         deref(v.l);
      elsif v.kind = V_LAMBDA then
         deref(v.lam);
      elsif v.kind = V_STACK then
         BBS.lisp.strings.deref(v.st_name);
      end if;
   end;
   --
end;
