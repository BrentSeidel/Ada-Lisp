with BBS.lisp.memory;
package body BBS.lisp.evaluate
with Refined_State =>  (pvt_exit_block => exit_block) is
   --
   --  Take an element_type and checks if it can be interpreted as true or false.
   --
   function isTrue(e : element_type) return Boolean is
   begin
      if e.kind = E_NIL then
         return False;
      elsif e.kind = E_VALUE then
         if e.v.kind = V_BOOLEAN  then
            return e.v.b;
         end if;
         return True;
      elsif e.kind = E_CONS then
         if (cons_table(e.ps).car.kind = E_NIL)
           and (cons_table(e.ps).cdr.kind = E_NIL) then
            return False;
         end if;
      end if;
      return True;
   end;
   --
   --  A list can be either element type E_CONS or a value of type V_LIST.  This
   --  checks both to see if the element is actually some sort of a list.
   --
   function isList(e : element_type) return Boolean is
   begin
      if e.kind = E_CONS then
         return True;
      end if;
      if e.kind = E_VALUE then
         if e.v.kind = V_LIST then
            return True;
         end if;
      end if;
      return False;
   end;
   --
   --  If e is list type, return the index of the head of the list, otherwise
   --  return NIL_CONS.
   --
   function getList(e : element_type) return cons_index is
   begin
      if e.kind = E_CONS then
         return e.ps;
      else
         if e.kind = E_VALUE and then e.v.kind = V_LIST then
            return e.v.l;
         end if;
      end if;
      return NIL_CONS;
   end;
   --
   --  This checks to see if the element represents a function call.  The element
   --  is a symbol of type either BUILTIN or LAMBDA.
   --
   function isFunction(e : element_type) return Boolean is
      temp : element_type;
      sym_kind : symbol_type;
      list : cons_index;
      val : value;
   begin
      list := getList(e);
      if list > NIL_CONS then
         temp := cons_table(list).car;
      else
         temp := e;
      end if;
      if temp.kind = E_SYMBOL then
         sym_kind := symb_table(temp.sym).kind;
         if (sym_kind = SY_BUILTIN) or
           (sym_kind = SY_SPECIAL) or
           (sym_kind = SY_LAMBDA) then
            return True;
         end if;
      elsif temp.kind = E_VALUE then
         if temp.v.kind = V_LAMBDA then
            return True;
         end if;
      elsif temp.kind = E_STACK then
         val := BBS.lisp.stack.search_frames(temp.st_offset, temp.st_name);
         if val.kind = V_LAMBDA then
            return True;
         end if;
      end if;
      return False;
   end;
   --
   --  Evaluate a list of statements.
   --
   function execute_block(e : element_type) return element_type is
      statement : element_type := e;
      ret_val   : element_type;
      list      : cons_index;
   begin
      --
      --  Evaluate the function
      --
      ret_val := NIL_ELEM;
      while isList(statement) loop
         BBS.lisp.memory.deref(ret_val);
         list := getList(statement);
         if isList(cons_table(list).car) then
            ret_val := eval_dispatch(getList(cons_table(list).car));
         else
            ret_val := indirect_elem(cons_table(list).car);
         end if;
         if ret_val.kind = E_ERROR then
            error("block execution", "Operation returned an error");
            exit;
         end if;
         if exit_block > 0 then
            exit;
         end if;
         statement := cons_table(list).cdr;
      end loop;
      return ret_val;
   end;
   --
   --  Set the exit_loop flag
   --
   procedure set_exit_block(n : Natural) is
   begin
      exit_block := n;
   end;
   --
   --  Decrement the exit_block flag
   --
   procedure decrement_exit_block is
   begin
      if exit_block > 0 then
         exit_block := exit_block - 1;
      end if;
   end;
   --
   --  Returns the exit_block flag
   --
   function get_exit_block return Natural is
   begin
      return exit_block;
   end;
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, it returns the atom that the variable points to.  If not, it
   --  just returns the atom.  If the variable points to a list, then the
   --  original atom is returned.
   --
   function indirect_elem(e : element_type) return element_type is
      sym : symb_index;
      val : value;
   begin
      if e.kind = E_SYMBOL then
         sym := e.sym;
         if symb_table(sym).kind = SY_VARIABLE then
            return symb_table(sym).pv;
         end if;
      end if;
      if e.kind = E_STACK then
         val := BBS.lisp.stack.search_frames(e.st_offset, e.st_name);
         return (kind => E_VALUE, v => val);
      end if;
      return e;
   end;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   function first_value(s : in out cons_index) return element_type is
      first : element_type;
   begin
      if s = NIL_CONS then
         return NIL_ELEM;
      else
         first := cons_table(s).car;
         s := getList(cons_table(s).cdr);
         if first.kind = E_NIL then
            null;
         elsif isList(first) then
            if isFunction(first) then
               first := eval_dispatch(getList(first));
            else
               BBS.lisp.memory.ref(first);
            end if;
         else
            first := indirect_elem(first);
            BBS.lisp.memory.ref(first);
         end if;
      end if;
      return first;
   end;
end;
