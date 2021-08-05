with BBS.lisp.memory;
package body BBS.lisp.conses is
   --
   --  Routines for accessing the cons table
   --
   function get_car(s : cons_index) return element_type is
   begin
      return cons_table(s).car;
   end;
   --
   procedure set_car(s : cons_index; e : element_type) is
   begin
      cons_table(s).car := e;
   end;
   --
   function get_cdr(s : cons_index) return element_type is
   begin
      return cons_table(s).cdr;
   end;
   --
   procedure set_cdr(s : cons_index; e : element_type) is
   begin
      cons_table(s).cdr := e;
   end;
   --
   function get_ref(s : cons_index) return cons_ref_count is
   begin
      return cons_table(s).ref;
   end;
   --  ------------------------------------------------------------------------
   --  Memory management
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
   --  Reset the cons table
   --
   procedure reset_cons_table is
   begin
      for i in cons_table'Range loop
         cons_table(i).ref := FREE_CONS;
         cons_table(i).car := NIL_ELEM;
         cons_table(i).cdr := NIL_ELEM;
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
            BBS.lisp.memory.deref(cons_table(s).car);
            BBS.lisp.memory.deref(cons_table(s).cdr);
            cons_table(s).car := NIL_ELEM;
            cons_table(s).cdr := NIL_ELEM;
         end if;
      end if;
   end;
end;
