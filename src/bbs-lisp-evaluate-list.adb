with BBS.lisp.memory;
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
      first_value(e, p1, t);
      if p1.kind = E_ERROR then
         error("cons", "Error reported evaluating first parameter.");
         return p1;
      end if;
      first_value(t, p2, t);
      if p2.kind = E_ERROR then
         error("cons", "Error reported evaluating second parameter.");
         return p2;
      end if;
      if BBS.lisp.memory.alloc(s) then
         cons_table(s).car := p1;
         cons_table(s).cdr := p2;
         BBS.lisp.memory.ref(p1);
         BBS.lisp.memory.ref(p2);
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
      first_value(e, first, rest);
      if isList(first) then
         s := getList(first);
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
      first_value(e, first, rest);
      if isList(first) then
         s := getList(first);
         temp := cons_table(s).cdr;
         BBS.lisp.memory.ref(temp);
         return temp;
      end if;
      return NIL_ELEM;
   end;
   --
   --  Create a list verbatum from the parameter list
   --
   function quote(e : element_type) return element_type is
   begin
      bbs.lisp.memory.ref(e);
      return e;
   end;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   function list(e : element_type) return element_type is
      first : element_type;
      rest : element_type := e;
      temp : element_type;
      head : cons_index;
      tail : cons_index;
      s : cons_index;
   begin
      if isList(e) then
         if BBS.lisp.memory.alloc(s) then
            first_value(e, first, rest);
            if first.kind = E_ERROR then
               error("list", "Parameter returned an error");
               return first;
            end if;
            BBS.lisp.memory.ref(first);
            cons_table(s).car := first;
            head := s;
            tail := s;
         else
            error("list", "Unable to allocate cons cell.");
            return (kind => E_ERROR);
         end if;
      else
         return NIL_ELEM;
      end if;
      while rest.kind /= E_NIL loop
         if BBS.lisp.memory.alloc(s) then
            first_value(rest, first, temp);
            if first.kind = E_ERROR then
               BBS.lisp.memory.deref(head);
               error("list", "Parameter returned an error");
               return first;
            end if;
            cons_table(tail).cdr := (kind => E_VALUE, v => (kind => V_LIST, l => s));
            tail := s;
            BBS.lisp.memory.ref(first);
            cons_table(s).car := first;
            rest := temp;
         else
            BBS.lisp.memory.deref(head);
            error("list", "Unable to allocate cons cell");
            return (kind => E_ERROR);
         end if;
      end loop;
      return (kind => E_VALUE, v => (kind => V_LIST, l => head));
   end;
   --
   --  Append one list to another.
   --
   function append(e : element_type) return element_type is
   begin
      return (kind => E_ERROR);
   end;

   --
end;
