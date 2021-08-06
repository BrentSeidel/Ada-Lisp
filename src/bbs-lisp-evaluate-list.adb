with BBS.lisp.conses;
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
         error("cons", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = V_ERROR then
         error("cons", "Error reported evaluating first parameter.");
         e := p1;
         return;
      end if;
      p2 := first_value(s1);
      if p2.kind = V_ERROR then
         error("cons", "Error reported evaluating second parameter.");
         e := p2;
         return;
      end if;
      if BBS.lisp.conses.alloc(s1) then
         BBS.lisp.conses.set_car(s1, p1);
         BBS.lisp.conses.set_cdr(s1, p2);
         BBS.lisp.memory.ref(p1);
         BBS.lisp.memory.ref(p2);
         e := (kind => V_LIST, l => s1);
      else
         error("cons", "Unable to allocate cons cell");
         e := make_error(ERR_ALLOCCONS);
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
         temp := BBS.lisp.conses.get_car(s1);
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
         temp := BBS.lisp.conses.get_cdr(s1);
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
      bbs.lisp.conses.ref(s);
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
         if BBS.lisp.conses.alloc(s1) then
            first := first_value(rest);
            if first.kind = V_ERROR then
               error("list", "Parameter returned an error");
               e := first;
               return;
            end if;
            BBS.lisp.memory.ref(first);
            BBS.lisp.conses.set_car(s1, first);
            head := s1;
            tail := s1;
         else
            error("list", "Unable to allocate initial cons cell.");
            e := make_error(ERR_ALLOCCONS);
            return;
         end if;
      else
         e := NIL_ELEM;
         return;
      end if;
      while rest > NIL_CONS loop
         if BBS.lisp.conses.alloc(s1) then
            first := first_value(rest);
            if first.kind = V_ERROR then
               BBS.lisp.conses.deref(head);
               error("list", "Parameter returned an error");
               e := first;
               return;
            end if;
            BBS.lisp.conses.set_cdr(tail, (kind => V_LIST, l => s1));
            tail := s1;
            BBS.lisp.memory.ref(first);
            BBS.lisp.conses.set_car(s1, first);
         else
            BBS.lisp.conses.deref(head);
            error("list", "Unable to allocate cons cell");
            e := make_error(ERR_ALLOCCONS);
            return;
         end if;
      end loop;
      e := (kind => V_LIST, l => head);
   end;
   --
   --  Replace the CAR of a list with a value
   --
   procedure rplaca(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      second : element_type;
      s1 : cons_index;
   begin
      if s = NIL_CONS then
         error("rplaca", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      first := first_value(rest);
      if not isList(first) then
         error("rplaca", "Must have a list to modify");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      second := first_value(rest);
      s1 := getList(first);
      if s1 > NIL_CONS then
         BBS.lisp.memory.deref(BBS.lisp.conses.get_car(s1));
         BBS.lisp.conses.set_car(s1, second);
      end if;
      e := first;
   end;
   --
   --  Replace the CDR of a list with a value
   --
   procedure rplacd(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      second : element_type;
      s1 : cons_index;
   begin
      if s = NIL_CONS then
         error("rplacd", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      first := first_value(rest);
      if not isList(first) then
         error("rplacd", "Must have a list to modify");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      second := first_value(rest);
      s1 := getList(first);
      if s1 > NIL_CONS then
         BBS.lisp.memory.deref(BBS.lisp.conses.get_cdr(s1));
         BBS.lisp.conses.set_cdr(s1, second);
      end if;
      e := first;
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
