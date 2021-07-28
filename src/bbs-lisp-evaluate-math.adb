with BBS.lisp.memory;

package body BBS.lisp.evaluate.math is
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(s : cons_index; b : mathops) return element_type is
      t : cons_index := s;
      accum : int32 := 0;
      v : value;
      el: element_type;
      --
      --  Subfunction to do the actual evaluation.
      --
      procedure process_value(i : int32; b : mathops) is
      begin
         case (b) is
            when PLUS =>
               accum := accum + i;
            when MUL =>
               accum := accum * i;
            when MINUS =>
               accum := accum - i;
            when DIV =>
               accum := accum / i;
         end case;
      end;

   begin
      if s = NIL_CONS then
         error("eval_math", "Internal error.  Should have a list.");
         return make_error(ERR_UNKNOWN);
      end if;
      el := first_value(t);
      if el.kind = E_VALUE then
         v := el.v;
      else
         error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
         BBS.lisp.memory.deref(el);
         return make_error(ERR_UNKNOWN);
      end if;
      if v.kind = V_INTEGER then
         accum := v.i;
      else
         error("eval_math", "Can't process value of type " & value_type'Image(v.kind));
         BBS.lisp.memory.deref(el);
         return make_error(ERR_UNKNOWN);
      end if;
      bbs.lisp.memory.deref(el);
      while t > NIL_CONS loop
         el := first_value(t);
         if el.kind = E_VALUE then
            v := el.v;
         else
            error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
            BBS.lisp.memory.deref(el);
            return make_error(ERR_UNKNOWN);
         end if;
         if v.kind /= V_INTEGER then
            error("eval_math", "Can't process value of type " & value_type'Image(v.kind));
            BBS.lisp.memory.deref(el);
            return make_error(ERR_UNKNOWN);
         end if;
         process_value(v.i, b);
         bbs.lisp.memory.deref(el);
      end loop;
      return (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum));
   end;
   --
   --
   --  Perform addition
   --
   procedure add(e : out element_type; s : cons_index) is
   begin
      e := eval_math(s, PLUS);
   end;
   --
   --  Perform subtraction
   --
   procedure sub(e : out element_type; s : cons_index) is
   begin
      e := eval_math(s, MINUS);
   end;
   --
   --  Perform multiplication
   --
   procedure mul(e : out element_type; s : cons_index) is
   begin
      e := eval_math(s, MUL);
   end;
   --
   --  Perform division
   --
   procedure div(e : out element_type; s : cons_index) is
   begin
      e := eval_math(s, DIV);
   end;
end;
