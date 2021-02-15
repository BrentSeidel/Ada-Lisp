with BBS.lisp.memory;
package body BBS.lisp.evaluate.bool is
   --
--   function eval_not(s : cons_index) return element_type is
   procedure eval_not(e : out element_type; s : cons_index) is
      p1 : element_type; --  Parameter
      s1 : cons_index := s;
      v : value;
   begin
      if s = NIL_CONS then
         error("eval_not", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = E_ERROR then
         error("eval_not", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("eval_not", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_BOOLEAN then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => not v.b));
         return;
      elsif v.kind = V_INTEGER then
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i =>
                                        uint32_to_int32(not int32_to_uint32(v.i))));
         return;
      else
         error("eval_not", "Cannot perform NOT of parameter of type " & value_type'Image(v.kind));
         e :=  (kind => E_ERROR);
         return;
      end if;
   end;
   --
--   function eval_and(s : cons_index) return element_type is
   procedure eval_and(e : out element_type; s : cons_index) is
      accum_i : int32 := -1;
      accum_b : Boolean := True;
      int_op : Boolean;
      ptr : cons_index;
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
      if s > NIL_CONS then
         ptr := s;
         temp := first_value(ptr);
         if accumulate(temp) = E_ERROR then
            error("eval_and", "Error processing parameter.");
            e := (kind => E_ERROR);
            return;
         end if;
         if ptr > NIL_CONS then
            if (int_op and (accum_i /= 0)) or ((not int_op) and accum_b) then
               loop
                  temp := first_value(ptr);
                  if accumulate(temp) = E_ERROR then
                     error("eval_and", "Error processing parameter.");
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  --
                  --  Check for short circuiting operations
                  --
                  exit when int_op and accum_i = 0;
                  exit when (not int_op) and (not accum_b);
                  --
                  --  Check for end of parameters
                  --
                  exit when ptr = NIL_CONS;
               end loop;
            end if;
         end if;
      else
         error("eval_and", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      if int_op then
         e := (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum_i));
      else
         e := (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => accum_b));
      end if;
   end;
   --
--   function eval_or(s : cons_index) return element_type is
   procedure eval_or(e : out element_type; s : cons_index) is
      accum_i : int32 := 0;
      accum_b : Boolean := False;
      int_op : Boolean;
      ptr : cons_index;
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
      if s > NIL_CONS then
         ptr := s;
         temp := first_value(ptr);
         if accumulate(temp) = E_ERROR then
            error("eval_ok", "Error processing parameter.");
            e := (kind => E_ERROR);
            return;
         end if;
         if ptr > NIL_CONS then
            if (int_op and (accum_i /= -1)) or ((not int_op) and (not accum_b)) then
               loop
                  temp := first_value(ptr);
                  if accumulate(temp) = E_ERROR then
                     error("eval_ok", "Error processing parameter.");
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  --
                  --  Check for short circuiting operations
                  --
                  exit when int_op and accum_i = -1;
                  exit when (not int_op) and accum_b;
                  --
                  --  Check for end of parameters
                  --
                  exit when ptr = NIL_CONS;
               end loop;
            end if;
         end if;
      else
         error("eval_or", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      if int_op then
         e := (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum_i));
      else
         e := (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => accum_b));
      end if;
   end;
   --
end;
