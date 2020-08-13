with BBS.lisp.memory;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.bool is
   function eval_not(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if e.kind /= E_CONS then
         error("eval_not", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      BBS.lisp.utilities.first_value(e, p1, t);
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
--      return (kind => E_ERROR);
   end;
   --
   function eval_and(e : element_type) return element_type is
      accum_i : int32;
      accum_b : Boolean;
      int_op : Boolean;
      ptr : element_type;
      temp : element_type;
      v : value;
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
            temp := BBS.lisp.utilities.indirect_elem(cons_table(ptr.ps).car);
            if temp.kind = E_VALUE then
               v := temp.v;
            else
               error("eval_and", "Can't process element " & ptr_type'Image(temp.kind));
               return (kind => E_ERROR);
            end if;
            if v.kind = V_INTEGER then
               accum_i := v.i;
               int_op := True;
            elsif v.kind = V_BOOLEAN then
               accum_b := v.b;
               int_op := False;
            else
               error("eval_and", "Can't process " & value_type'Image(v.kind));
               BBS.lisp.memory.deref(temp);
               return (kind => E_ERROR);
            end if;
         else  -- It is E_CONS
            temp := eval_dispatch(cons_table(ptr.ps).car.ps);
            if temp.kind /= E_CONS then
               temp := bbs.lisp.utilities.indirect_elem(temp);
               if temp.kind = E_VALUE then
                  v := temp.v;
               end if;
               if v.kind = V_INTEGER then
                  accum_i := v.i;
                  int_op := True;
               elsif v.kind = V_BOOLEAN then
                  accum_b := v.b;
                  int_op := False;
               else
                  error("eval_and", "Can't process " & value_type'Image(v.kind));
                  BBS.lisp.memory.deref(temp);
                  return (kind => E_ERROR);
               end if;
            end if;
            bbs.lisp.memory.deref(temp);
         end if;
         if cons_table(ptr.ps).cdr.kind /= E_NIL then
            ptr := cons_table(ptr.ps).cdr;
            loop
               if cons_table(ptr.ps).car.kind = E_CONS then
                  temp := eval_dispatch(cons_table(ptr.ps).car.ps);
                  if temp.kind = E_VALUE then
                     v := temp.v;
                     if int_op then
                        if v.kind = V_INTEGER then
                           accum_i := uint32_to_int32(int32_to_uint32(accum_i) and
                                                        int32_to_uint32(v.i));
                        else
                           error("eval_and", "Can't mix integers and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     else
                        if v.kind = V_BOOLEAN then
                           accum_b := accum_b and v.b;
                        else
                           error("eval_and", "Can't mix booleans and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     end if;
                  elsif temp.kind = E_ERROR then
                     error("eval_and", "Argument evaluation returned an error");
                     return temp;
                  else
                     error("eval_and", "Cannot process argument " & ptr_type'Image(temp.kind));
                     bbs.lisp.memory.deref(temp);
                     return (kind => E_ERROR);
                  end if;
               else
                  temp := bbs.lisp.utilities.indirect_elem(cons_table(ptr.ps).car);
                  if temp.kind = E_VALUE then
                     v := temp.v;
                     if int_op then
                        if v.kind = V_INTEGER then
                           accum_i := uint32_to_int32(int32_to_uint32(accum_i) and
                                                        int32_to_uint32(v.i));
                        else
                           error("eval_and", "Can't mix integers and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     else
                        if v.kind = V_BOOLEAN then
                           accum_b := accum_b and v.b;
                        else
                           error("eval_and", "Can't mix booleans and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     end if;
                  elsif temp.kind = E_ERROR then
                     error("eval_and", "Argument evaluation returned an error");
                     return temp;
                  else
                     error("eval_and", "Cannot process argument " & ptr_type'Image(temp.kind));
                     bbs.lisp.memory.deref(temp);
                     return (kind => E_ERROR);
                  end if;
               end if;
               exit when cons_table(ptr.ps).cdr.kind /= E_CONS;
               ptr := cons_table(ptr.ps).cdr;
            end loop;
            if cons_table(ptr.ps).cdr.kind /= E_NIL then
               temp := bbs.lisp.utilities.indirect_elem(cons_table(ptr.ps).cdr);
               if temp.kind = E_VALUE then
                  v := temp.v;
                  if int_op then
                     if v.kind = V_INTEGER then
                        accum_i := uint32_to_int32(int32_to_uint32(accum_i) and
                                                     int32_to_uint32(v.i));
                     else
                        error("eval_and", "Can't mix integers and " & value_type'Image(v.kind));
                        BBS.lisp.memory.deref(temp);
                        return (kind => E_ERROR);
                     end if;
                  else
                     if v.kind = V_BOOLEAN then
                        accum_b := accum_b and v.b;
                     else
                        error("eval_and", "Can't mix booleans and " & value_type'Image(v.kind));
                        BBS.lisp.memory.deref(temp);
                        return (kind => E_ERROR);
                     end if;
                  end if;
               elsif temp.kind = E_ERROR then
                  error("eval_and", "Argument evaluation returned an error");
                  return temp;
               else
                  error("eval_and", "Cannot process argument " & ptr_type'Image(temp.kind));
                  bbs.lisp.memory.deref(temp);
                  return (kind => E_ERROR);
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
      accum_i : int32;
      accum_b : Boolean;
      int_op : Boolean;
      ptr : element_type;
      temp : element_type;
      v : value;
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
            temp := BBS.lisp.utilities.indirect_elem(cons_table(ptr.ps).car);
            if temp.kind = E_VALUE then
               v := temp.v;
            else
               error("eval_or", "Can't process element " & ptr_type'Image(temp.kind));
               return (kind => E_ERROR);
            end if;
            if v.kind = V_INTEGER then
               accum_i := v.i;
               int_op := True;
            elsif v.kind = V_BOOLEAN then
               accum_b := v.b;
               int_op := False;
            else
               error("eval_or", "Can't process " & value_type'Image(v.kind));
               BBS.lisp.memory.deref(temp);
               return (kind => E_ERROR);
            end if;
         else  -- It is E_CONS
            temp := eval_dispatch(cons_table(ptr.ps).car.ps);
            if temp.kind /= E_CONS then
               temp := bbs.lisp.utilities.indirect_elem(temp);
               if temp.kind = E_VALUE then
                  v := temp.v;
               end if;
               if v.kind = V_INTEGER then
                  accum_i := v.i;
                  int_op := True;
               elsif v.kind = V_BOOLEAN then
                  accum_b := v.b;
                  int_op := False;
               else
                  error("eval_or", "Can't process " & value_type'Image(v.kind));
                  BBS.lisp.memory.deref(temp);
                  return (kind => E_ERROR);
               end if;
            end if;
            bbs.lisp.memory.deref(temp);
         end if;
         if cons_table(ptr.ps).cdr.kind /= E_NIL then
            ptr := cons_table(ptr.ps).cdr;
            loop
               if cons_table(ptr.ps).car.kind = E_CONS then
                  temp := eval_dispatch(cons_table(ptr.ps).car.ps);
                  if temp.kind = E_VALUE then
                     v := temp.v;
                     if int_op then
                        if v.kind = V_INTEGER then
                           accum_i := uint32_to_int32(int32_to_uint32(accum_i) or
                                                        int32_to_uint32(v.i));
                        else
                           error("eval_or", "Can't mix integers and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     else
                        if v.kind = V_BOOLEAN then
                           accum_b := accum_b or v.b;
                        else
                           error("eval_or", "Can't mix booleans and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     end if;
                  elsif temp.kind = E_ERROR then
                     error("eval_or", "Argument evaluation returned an error");
                     return temp;
                  else
                     error("eval_or", "Cannot process argument " & ptr_type'Image(temp.kind));
                     bbs.lisp.memory.deref(temp);
                     return (kind => E_ERROR);
                  end if;
               else
                  temp := bbs.lisp.utilities.indirect_elem(cons_table(ptr.ps).car);
                  if temp.kind = E_VALUE then
                     v := temp.v;
                     if int_op then
                        if v.kind = V_INTEGER then
                           accum_i := uint32_to_int32(int32_to_uint32(accum_i) or
                                                        int32_to_uint32(v.i));
                        else
                           error("eval_or", "Can't mix integers and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     else
                        if v.kind = V_BOOLEAN then
                           accum_b := accum_b or v.b;
                        else
                           error("eval_or", "Can't mix booleans and " & value_type'Image(v.kind));
                           BBS.lisp.memory.deref(temp);
                           return (kind => E_ERROR);
                        end if;
                     end if;
                  elsif temp.kind = E_ERROR then
                     error("eval_or", "Argument evaluation returned an error");
                     return temp;
                  else
                     error("eval_or", "Cannot process argument " & ptr_type'Image(temp.kind));
                     bbs.lisp.memory.deref(temp);
                     return (kind => E_ERROR);
                  end if;
               end if;
               exit when cons_table(ptr.ps).cdr.kind /= E_CONS;
               ptr := cons_table(ptr.ps).cdr;
            end loop;
            if cons_table(ptr.ps).cdr.kind /= E_NIL then
               temp := bbs.lisp.utilities.indirect_elem(cons_table(ptr.ps).cdr);
               if temp.kind = E_VALUE then
                  v := temp.v;
                  if int_op then
                     if v.kind = V_INTEGER then
                        accum_i := uint32_to_int32(int32_to_uint32(accum_i) or
                                                     int32_to_uint32(v.i));
                     else
                        error("eval_or", "Can't mix integers and " & value_type'Image(v.kind));
                        BBS.lisp.memory.deref(temp);
                        return (kind => E_ERROR);
                     end if;
                  else
                     if v.kind = V_BOOLEAN then
                        accum_b := accum_b or v.b;
                     else
                        error("eval_or", "Can't mix booleans and " & value_type'Image(v.kind));
                        BBS.lisp.memory.deref(temp);
                        return (kind => E_ERROR);
                     end if;
                  end if;
               elsif temp.kind = E_ERROR then
                  error("eval_or", "Argument evaluation returned an error");
                  return temp;
               else
                  error("eval_or", "Cannot process argument " & ptr_type'Image(temp.kind));
                  bbs.lisp.memory.deref(temp);
                  return (kind => E_ERROR);
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
