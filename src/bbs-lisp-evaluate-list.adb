with BBS.lisp.memory;
package body BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
--   function cons(s : cons_index) return element_type is
   procedure cons(e : out element_type; s : cons_index) is
      s1 : cons_index := s;
      p1 : element_type;  --  First parameter
      p2 : element_type;  --  Second parameter
   begin
      if s = NIL_CONS then
         error("cons(e", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = E_ERROR then
         error("cons", "Error reported evaluating first parameter.");
         e := p1;
         return;
      end if;
      p2 := first_value(s1);
      if p2.kind = E_ERROR then
         error("cons", "Error reported evaluating second parameter.");
         e := p2;
         return;
      end if;
      if BBS.lisp.memory.alloc(s1) then
         cons_table(s1).car := p1;
         cons_table(s1).cdr := p2;
         BBS.lisp.memory.ref(p1);
         BBS.lisp.memory.ref(p2);
         e := (kind => E_VALUE, v => (kind => V_LIST, l => s));
      else
         error("cons", "Unable to allocate cons cell");
         e := (kind => E_ERROR);
      end if;
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
--   function car(s : cons_index) return element_type is
   procedure car(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      temp : element_type;
      s1 : cons_index;
   begin
      first := first_value(rest);
      if isList(first) then
         s1 := getList(first);
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
--   function cdr(s : cons_index) return element_type is
   procedure cdr(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      first : element_type;
      temp : element_type;
      s1 : cons_index;
   begin
      first := first_value(rest);
      if isList(first) then
         s1 := getList(first);
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
--   function quote(s : cons_index) return element_type is
   procedure quote(e : out element_type; s : cons_index) is
      t : constant element_type := (kind => E_CONS, ps => s);
   begin
      bbs.lisp.memory.ref(t);
      e := t;
   end;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
--   function list(s : cons_index) return element_type is
   procedure list(e : out element_type; s : cons_index) is
      first : element_type;
      rest : cons_index := s;
      head : cons_index;
      tail : cons_index;
      s1 : cons_index;
   begin
      if s > cons_index'First then
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if first.kind = E_ERROR then
               error("list", "Parameter returned an error");
               e := first;
               return;
            end if;
            BBS.lisp.memory.ref(first);
            cons_table(s1).car := first;
            head := s1;
            tail := s1;
         else
            error("list", "Unable to allocate cons cell.");
            e := (kind => E_ERROR);
            return;
         end if;
      else
         e := NIL_ELEM;
         return;
      end if;
      while rest > NIL_CONS loop
         if BBS.lisp.memory.alloc(s1) then
            first := first_value(rest);
            if first.kind = E_ERROR then
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
            e := (kind => E_ERROR);
            return;
         end if;
      end loop;
      e := (kind => E_VALUE, v => (kind => V_LIST, l => head));
   end;
   --
   --  Append one list to another.
   --
--   function append(s : cons_index) return element_type is
   procedure append(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      e := (kind => E_ERROR);
   end;

   --
end;
