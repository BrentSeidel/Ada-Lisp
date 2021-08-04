with BBS.lisp.memory;

package body BBS.lisp.evaluate.math is
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(s : cons_index; b : mathops) return element_type is
      t : cons_index := s;
      accum : int32 := 0;
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
         error("eval_math", "No parameters provided.");
         return make_error(ERR_NOPARAM);
      end if;
      el := first_value(t);
      if el.kind = V_INTEGER then
         accum := el.i;
      else
         error("eval_math", "Can't process value of type " & value_type'Image(el.kind));
         BBS.lisp.memory.deref(el);
         return make_error(ERR_WRONGTYPE);
      end if;
      bbs.lisp.memory.deref(el);
      while t > NIL_CONS loop
         el := first_value(t);
         if el.kind /= V_INTEGER then
            error("eval_math", "Can't process value of type " & value_type'Image(el.kind));
            BBS.lisp.memory.deref(el);
            return make_error(ERR_WRONGTYPE);
         end if;
         process_value(el.i, b);
         bbs.lisp.memory.deref(el);
      end loop;
      return (kind => V_INTEGER, i => accum);
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
