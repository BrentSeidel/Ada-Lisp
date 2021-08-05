with BBS.lisp.memory;
package body BBS.lisp.evaluate.bool is
   --
   procedure eval_not(e : out element_type; s : cons_index) is
      p1 : element_type; --  Parameter
      s1 : cons_index := s;
   begin
      if s = NIL_CONS then
         error("eval_not", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = V_ERROR then
         error("eval_not", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_BOOLEAN then
         e := (kind => V_BOOLEAN, b => not p1.b);
      elsif p1.kind = V_INTEGER then
         e := (kind => V_INTEGER, i => uint32_to_int32(not int32_to_uint32(p1.i)));
      else
         error("eval_not", "Cannot perform NOT of parameter of type " & value_type'Image(p1.kind));
         e :=  make_error(ERR_WRONGTYPE);
      end if;
   end;
   --
   procedure eval_and(e : out element_type; s : cons_index) is
      accum_i : int32 := -1;
      accum_b : Boolean := True;
      int_op : Boolean := False;
      ptr : cons_index;
      temp : element_type;

      function accumulate(t : element_type; first : Boolean) return value_type is
      begin
         if (t.kind = V_INTEGER and first) or (t.kind = V_INTEGER and int_op) then
            accum_i := uint32_to_int32(int32_to_uint32(accum_i) and
                                                        int32_to_uint32(t.i));
            int_op := True;
         elsif (t.kind = V_BOOLEAN and first) or (t.kind = V_BOOLEAN and not int_op) then
            accum_b := accum_b and t.b;
            int_op := False;
         else
            error("eval_and", "Can't process " & value_type'Image(t.kind));
            BBS.lisp.memory.deref(temp);
            return V_ERROR;
         end if;
         return V_NONE;
      end;
      --
   begin
      if s > NIL_CONS then
         ptr := s;
         temp := first_value(ptr);
         if accumulate(temp, True) = V_ERROR then
            error("eval_and", "Error processing parameter.");
            e := make_error(ERR_WRONGTYPE);
            return;
         end if;
         if ptr > NIL_CONS then
            if (int_op and (accum_i /= 0)) or ((not int_op) and accum_b) then
               loop
                  temp := first_value(ptr);
                  if accumulate(temp, False) = V_ERROR then
                     error("eval_and", "Error processing parameter.");
                     e := make_error(ERR_WRONGTYPE);
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
         error("eval_and", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      if int_op then
         e := (kind => V_INTEGER, i => accum_i);
      else
         e := (kind => V_BOOLEAN, b => accum_b);
      end if;
   end;
   --
   procedure eval_or(e : out element_type; s : cons_index) is
      accum_i : int32 := 0;
      accum_b : Boolean := False;
      int_op : Boolean := False;
      ptr : cons_index;
      temp : element_type;

      function accumulate(t : element_type; first : Boolean) return value_type is
      begin
         if (t.kind = V_INTEGER and first) or (t.kind = V_INTEGER and int_op) then
            accum_i := uint32_to_int32(int32_to_uint32(accum_i) or
                                                        int32_to_uint32(t.i));
            int_op := True;
         elsif (t.kind = V_BOOLEAN and first) or (t.kind = V_BOOLEAN and not int_op) then
            accum_b := accum_b or t.b;
            int_op := False;
         else
            error("eval_or", "Can't process " & value_type'Image(t.kind));
            BBS.lisp.memory.deref(temp);
            return V_ERROR;
         end if;
         return V_NONE;
      end;
      --
   begin
      if s > NIL_CONS then
         ptr := s;
         temp := first_value(ptr);
         if accumulate(temp, True) = V_ERROR then
            error("eval_or", "Error processing parameter.");
            e := make_error(ERR_WRONGTYPE);
            return;
         end if;
         if ptr > NIL_CONS then
            if (int_op and (accum_i /= -1)) or ((not int_op) and (not accum_b)) then
               loop
                  temp := first_value(ptr);
                  if accumulate(temp, False) = V_ERROR then
                     error("eval_or", "Error processing parameter.");
                     e := make_error(ERR_WRONGTYPE);
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
         error("eval_or", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      if int_op then
         e := (kind => V_INTEGER, i => accum_i);
      else
         e := (kind => V_BOOLEAN, b => accum_b);
      end if;
   end;
   --
end;
