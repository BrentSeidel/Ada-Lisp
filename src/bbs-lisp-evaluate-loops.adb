with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.utilities;
with BBS.lisp.stack;
package body BBS.lisp.evaluate.loops is
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   function dowhile(e : element_type) return element_type is
      cond : element_type; --  Condition to evaluate
      list : element_type; --  List of operations to execute
      ptr : element_type;
      t : element_type := NIL_ELEM;
      temp : element_type;
   begin
      if e.kind = E_CONS then
         cond := cons_table(e.ps).car;
         list := cons_table(e.ps).cdr;
         --
         --  Loop while the conditions is true.
         --
         temp := eval_dispatch(cond.ps);
         while bbs.lisp.utilities.isTrue(temp) loop
            BBS.lisp.memory.deref(temp);
            ptr := list;
            --
            --  Evaluate all of the items in the list.
            --
            while ptr.kind /= E_NIL loop
               BBS.lisp.memory.deref(t);
               if ptr.kind = E_CONS then
                  if cons_table(ptr.ps).car.kind = E_CONS then
                     t := eval_dispatch(cons_table(ptr.ps).car.ps);
                  else
                     t := cons_table(ptr.ps).car;
                     BBS.lisp.memory.ref(t);
                  end if;
                  ptr := cons_table(ptr.ps).cdr;
               else
                  t := ptr;
                  BBS.lisp.memory.ref(t);
                  ptr := NIL_ELEM;
               end if;
            end loop;
            temp := eval_dispatch(cond.ps);
         end loop;
         BBS.lisp.memory.deref(temp);
      else
         error("dowhile", "Must provide a condition and expressions.");
      end if;
      return t;
   end;
   --
   function dotimes(e : element_type) return element_type is
   begin
      return NIL_ELEM;
   end;
end;
