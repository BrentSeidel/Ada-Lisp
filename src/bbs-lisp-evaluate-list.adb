with BBS.lisp.memory;
package body BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   procedure cons(e : out element_type; s : cons_index) is
      s1 : cons_index := s;
      p1 : element_type;  --  First parameter
      p2 : element_type;  --  Second parameter
   begin
      if s = NIL_CONS then
         error("cons", "Internal error.  Should have a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p1 := first_value(s1);
      if (p1.kind = E_VALUE) and then (p1.v.kind = V_ERROR) then
         error("cons", "Error reported evaluating first parameter.");
         e := p1;
         return;
      end if;
      p2 := first_value(s1);
      if (p2.kind = E_VALUE) and then (p2.v.kind = V_ERROR) then
         error("cons", "Error reported evaluating second parameter.");
         e := p2;
         return;
      end if;
      if BBS.lisp.memory.alloc(s1) then
         cons_table(s1).car := p1;
         cons_table(s1).cdr := p2;
         BBS.lisp.memory.ref(p1);
         BBS.lisp.memory.ref(p2);
         e := (kind => E_VALUE, v => (kind => V_LIST, l => s1));
      else
         error("cons", "Unable to allocate cons cell");
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
   procedure car(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      temp : element_type;
      s1 : cons_index;
   begin
      first := first_value(rest);
      s1 := getList(first);
      if s1 > NIL_CONS then
         temp := cons_table(s1).car;
         BBS.lisp.memory.ref(temp);
         e := temp;
      else
         e := first;
      end if;
   end;
   --
   --  Return the rest of a list
   --
   procedure cdr(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      temp : element_type;
      s1 : cons_index;
   begin
      first := first_value(rest);
      s1 := getList(first);
      if s1 > NIL_CONS then
         temp := cons_table(s1).cdr;
         BBS.lisp.memory.ref(temp);
         e := temp;
      else
         e := NIL_ELEM;
      end if;
   end;
   --
   --  Create a list verbatum from the parameter list
   --
   procedure quote(e : out element_type; s : cons_index) is
   begin
      bbs.lisp.memory.ref(s);
      e := makeList(s);
   end;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   procedure list(e : out element_type; s : cons_index) is
      first : element_type;
      rest : cons_index := s;
      head : cons_index;
      tail : cons_index;
      s1 : cons_index;
   begin
      if s > NIL_CONS then
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if (first.kind = E_VALUE) and then (first.v.kind = V_ERROR) then
               error("list", "Parameter returned an error");
               e := first;
               return;
            end if;
            BBS.lisp.memory.ref(first);
            cons_table(s1).car := first;
            head := s1;
            tail := s1;
         else
            error("list", "Unable to allocate initial cons cell.");
            e := make_error(ERR_UNKNOWN);
            return;
         end if;
      else
         e := NIL_ELEM;
         return;
      end if;
      while rest > NIL_CONS loop
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if (first.kind = E_VALUE) and then (first.v.kind = V_ERROR) then
               BBS.lisp.memory.deref(head);
               error("list", "Parameter returned an error");
               e := first;
               return;
            end if;
            cons_table(tail).cdr := (kind => E_VALUE, v => (kind => V_LIST, l => s1));
            tail := s1;
            BBS.lisp.memory.ref(first);
            cons_table(s1).car := first;
         else
            BBS.lisp.memory.deref(head);
            error("list", "Unable to allocate cons cell");
            e := make_error(ERR_UNKNOWN);
            return;
         end if;
      end loop;
      e := (kind => E_VALUE, v => (kind => V_LIST, l => head));
   end;
   --
   --  Append one list to another.  Not yet implemented.
   --
--   procedure append(e : out element_type; s : cons_index) is
--      pragma Unreferenced (s);
--   begin
--      e := (kind => E_ERROR);
--   end;
   --
end;
