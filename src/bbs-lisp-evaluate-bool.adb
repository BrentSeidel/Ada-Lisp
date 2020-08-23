with BBS.lisp.memory;
package body BBS.lisp.evaluate.bool is
   --
   function eval_not(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if e.kind /= E_CONS then
         error("eval_not", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      first_value(e, p1, t);
      if p1.kind = E_ERROR then
         error("eval_not", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("eval_not", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_BOOLEAN then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => not v.b));
      elsif v.kind = V_INTEGER then
         return (kind => E_VALUE, v => (kind => V_INTEGER, i =>
                                          uint32_to_int32(not int32_to_uint32(v.i))));
      else
         error("eval_not", "Cannot perform NOT of parameter of type " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
   function eval_and(e : element_type) return element_type is
      accum_i : int32 := -1;
      accum_b : Boolean := True;
      int_op : Boolean;
      ptr : element_type;
      temp : element_type;

      function accumulate(t : element_type) return ptr_type is
         v : value;
      begin
         if t.kind = E_VALUE then
            v := t.v;
         else
            error("eval_and", "Can't process element " & ptr_type'Image(temp.kind));
            BBS.lisp.memory.deref(temp);
            return E_ERROR;
         end if;
         if v.kind = V_INTEGER then
            accum_i := uint32_to_int32(int32_to_uint32(accum_i) and
                                                        int32_to_uint32(v.i));
            int_op := True;
         elsif v.kind = V_BOOLEAN then
            accum_b := accum_b and v.b;
            int_op := False;
         else
            error("eval_and", "Can't process " & value_type'Image(v.kind));
            BBS.lisp.memory.deref(temp);
            return E_ERROR;
         end if;
         return E_NIL;
      end;
      --
   begin
      if e.kind = E_VALUE then
         if e.v.kind = V_INTEGER then
            accum_i := e.v.i;
            int_op := True;
         elsif e.v.kind = V_BOOLEAN then
            accum_b := e.v.b;
            int_op := False;
         else
            error("eval_and", "Can only operate on integers and booleans.");
            return (kind => E_ERROR);
         end if;
      elsif e.kind = E_CONS then
         ptr := e;
         if cons_table(ptr.ps).car.kind /= E_CONS then
            temp := indirect_elem(cons_table(ptr.ps).car);
         else  -- It is E_CONS
            temp := eval_dispatch(cons_table(ptr.ps).car.ps);
         end if;
         if accumulate(temp) = E_ERROR then
            return (kind => E_ERROR);
         end if;
         if cons_table(ptr.ps).cdr.kind /= E_NIL then
            ptr := cons_table(ptr.ps).cdr;
            if (int_op and (accum_i /= 0)) or ((not int_op) and accum_b) then
               loop
                  if cons_table(ptr.ps).car.kind = E_CONS then
                     temp := eval_dispatch(cons_table(ptr.ps).car.ps);
                  else
                     temp := indirect_elem(cons_table(ptr.ps).car);
                  end if;
                  if accumulate(temp) = E_ERROR then
                     return (kind => E_ERROR);
                  end if;
                  --
                  --  Check for short circuiting operations
                  --
                  exit when int_op and accum_i = 0;
                  exit when (not int_op) and (not accum_b);
                  --
                  --  Check for end of parameters
                  --
                  exit when cons_table(ptr.ps).cdr.kind /= E_CONS;
                  ptr := cons_table(ptr.ps).cdr;
               end loop;
            end if;
            if (int_op and (accum_i /= 0)) or ((not int_op) and accum_b) then
               if cons_table(ptr.ps).cdr.kind /= E_NIL then
                  temp := indirect_elem(cons_table(ptr.ps).cdr);
                  if accumulate(temp) = E_ERROR then
                     return (kind => E_ERROR);
                  end if;
               end if;
            end if;
         end if;
      else
         error("eval_and", "Can't operate on " & ptr_type'Image(e.kind));
         return (kind => E_ERROR);
      end if;
      if int_op then
         return (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum_i));
      else
         return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => accum_b));
      end if;
   end;
   --
   function eval_or(e : element_type) return element_type is
      accum_i : int32 := 0;
      accum_b : Boolean := False;
      int_op : Boolean;
      ptr : element_type;
      temp : element_type;

      function accumulate(t : element_type) return ptr_type is
         v : value;
      begin
         if t.kind = E_VALUE then
            v := t.v;
         else
            error("eval_or", "Can't process element " & ptr_type'Image(temp.kind));
            BBS.lisp.memory.deref(temp);
            return E_ERROR;
         end if;
         if v.kind = V_INTEGER then
            accum_i := uint32_to_int32(int32_to_uint32(accum_i) or
                                                        int32_to_uint32(v.i));
            int_op := True;
         elsif v.kind = V_BOOLEAN then
            accum_b := accum_b or v.b;
            int_op := False;
         else
            error("eval_or", "Can't process " & value_type'Image(v.kind));
            BBS.lisp.memory.deref(temp);
            return E_ERROR;
         end if;
         return E_NIL;
      end;
      --
   begin
      if e.kind = E_VALUE then
         if e.v.kind = V_INTEGER then
            accum_i := e.v.i;
            int_op := True;
         elsif e.v.kind = V_BOOLEAN then
            accum_b := e.v.b;
            int_op := False;
         else
            error("eval_or", "Can only operate on integers and booleans.");
            return (kind => E_ERROR);
         end if;
      elsif e.kind = E_CONS then
         ptr := e;
         if cons_table(ptr.ps).car.kind /= E_CONS then
            temp := indirect_elem(cons_table(ptr.ps).car);
            if accumulate(temp) = E_ERROR then
               return (kind => E_ERROR);
            end if;
         else  -- It is E_CONS
            temp := eval_dispatch(cons_table(ptr.ps).car.ps);
         end if;
         if accumulate(temp) = E_ERROR then
            return (kind => E_ERROR);
         end if;
         if cons_table(ptr.ps).cdr.kind /= E_NIL then
            ptr := cons_table(ptr.ps).cdr;
            if (int_op and (accum_i /= -1)) or ((not int_op) and (not accum_b)) then
               loop
                  if cons_table(ptr.ps).car.kind = E_CONS then
                     temp := eval_dispatch(cons_table(ptr.ps).car.ps);
                  else
                     temp := indirect_elem(cons_table(ptr.ps).car);
                  end if;
                  if accumulate(temp) = E_ERROR then
                     return (kind => E_ERROR);
                  end if;
                  --
                  --  Check for short circuiting operations
                  --
                  exit when int_op and accum_i = -1;
                  exit when (not int_op) and accum_b;
                  --
                  --  Check for end of parameters
                  --
                  exit when cons_table(ptr.ps).cdr.kind /= E_CONS;
                  ptr := cons_table(ptr.ps).cdr;
               end loop;
            end if;
            if (int_op and (accum_i /= -1)) or ((not int_op) and (not accum_b)) then
               if cons_table(ptr.ps).cdr.kind /= E_NIL then
                  temp := indirect_elem(cons_table(ptr.ps).cdr);
                  if accumulate(temp) = E_ERROR then
                     return (kind => E_ERROR);
                  end if;
               end if;
            end if;
         end if;
      else
         error("eval_or", "Can't operate on " & ptr_type'Image(e.kind));
         return (kind => E_ERROR);
      end if;
      if int_op then
         return (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum_i));
      else
         return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => accum_b));
      end if;
   end;
   --
end;
