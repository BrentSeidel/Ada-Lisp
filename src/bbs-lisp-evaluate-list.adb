with BBS.lisp.memory;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   function cons(e : element_type) return element_type is
      p1 : element_type;  --  First parameter
      p2 : element_type;  --  Second parameter
      t  : element_type;  --  Temporary value
      s  : cons_index;    --  Created cons cell
   begin
      if e.kind /= E_CONS then
         error("cons(e", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      BBS.lisp.utilities.first_value(e, p1, t);
      if p1.kind = E_ERROR then
         error("cons", "Error reported evaluating first parameter.");
         return p1;
      end if;
      BBS.lisp.utilities.first_value(t, p2, t);
      if p2.kind = E_ERROR then
         error("cons", "Error reported evaluating second parameter.");
         return p2;
      end if;
      if BBS.lisp.memory.alloc(s) then
         cons_table(s).car := p1;
         cons_table(s).cdr := p2;
         return (kind => E_VALUE, v => (kind => V_LIST, l => s));
      else
         error("cons", "Unable to allocate cons cell");
         return (kind => E_ERROR);
      end if;
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
   function car(e : element_type) return element_type is
      first : element_type;
      temp : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         temp := cons_table(s).car;
         BBS.lisp.memory.ref(temp);
         return temp;
      end if;
      return first;
   end;
   --
   --  Return the rest of a list
   --
   function cdr(e : element_type) return element_type is
      first : element_type;
      temp : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         temp := cons_table(s).cdr;
         BBS.lisp.memory.ref(temp);
         return temp;
      end if;
      return NIL_ELEM;
   end;
   --
end;
