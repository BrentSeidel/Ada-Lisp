with BBS.lisp.memory;
package body BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   function cons(s : cons_index) return element_type is
      t  : element_type := (kind => E_CONS, ps => s);
      p1 : element_type;  --  First parameter
      p2 : element_type;  --  Second parameter
      s1 : cons_index;    --  Created cons cell
   begin
      if s = cons_index'First then
         error("cons(e", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("cons", "Error reported evaluating first parameter.");
         return p1;
      end if;
      p2 := first_value(t);
      if p2.kind = E_ERROR then
         error("cons", "Error reported evaluating second parameter.");
         return p2;
      end if;
      if BBS.lisp.memory.alloc(s1) then
         cons_table(s1).car := p1;
         cons_table(s1).cdr := p2;
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
   function car(s : cons_index) return element_type is
      first : element_type;
      temp : element_type;
      rest : element_type := (kind => E_CONS, ps => s);
      s1 : cons_index;
   begin
      first := first_value(rest);
      if isList(first) then
         s1 := getList(first);
         temp := cons_table(s1).car;
         BBS.lisp.memory.ref(temp);
         return temp;
      end if;
      return first;
   end;
   --
   --  Return the rest of a list
   --
   function cdr(s : cons_index) return element_type is
      first : element_type;
      temp : element_type;
      rest : element_type := (kind => E_CONS, ps => s);
      s1 : cons_index;
   begin
      first := first_value(rest);
      if isList(first) then
         s1 := getList(first);
         temp := cons_table(s1).cdr;
         BBS.lisp.memory.ref(temp);
         return temp;
      end if;
      return NIL_ELEM;
   end;
   --
   --  Create a list verbatum from the parameter list
   --
   function quote(s : cons_index) return element_type is
      e : constant element_type := (kind => E_CONS, ps => s);
   begin
      bbs.lisp.memory.ref(e);
      return e;
   end;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   function list(s : cons_index) return element_type is
      first : element_type;
      rest : element_type := (kind => E_CONS, ps => s);
      head : cons_index;
      tail : cons_index;
      s1 : cons_index;
   begin
      if s > cons_index'First then
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if first.kind = E_ERROR then
               error("list", "Parameter returned an error");
               return first;
            end if;
            BBS.lisp.memory.ref(first);
            cons_table(s1).car := first;
            head := s1;
            tail := s1;
         else
            error("list", "Unable to allocate cons cell.");
            return (kind => E_ERROR);
         end if;
      else
         return NIL_ELEM;
      end if;
      while rest.kind /= E_NIL loop
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if first.kind = E_ERROR then
               BBS.lisp.memory.deref(head);
               error("list", "Parameter returned an error");
               return first;
            end if;
            cons_table(tail).cdr := (kind => E_VALUE, v => (kind => V_LIST, l => s1));
            tail := s1;
            BBS.lisp.memory.ref(first);
            cons_table(s1).car := first;
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
   function append(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      return (kind => E_ERROR);
   end;

   --
end;
