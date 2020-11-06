with BBS.lisp.memory;

package body BBS.lisp.evaluate.math is
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(e : element_type; b : mathops) return element_type is
      t : element_type := e;
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
      if e.kind = E_VALUE then
         if e.v.kind = V_INTEGER then
            return (Kind => E_VALUE, v => (kind => V_INTEGER, i => e.v.i));
         else
            error("eval_math", "Internal error.  Not an integer.");
            BBS.lisp.memory.deref(e);
            return (kind => E_ERROR);
         end if;
      elsif e.kind /= E_CONS then
         error("eval_math", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      el := first_value(t);
      if el.kind = E_VALUE then
         v := el.v;
      else
         error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
         return (kind => E_ERROR);
      end if;
      if v.kind = V_INTEGER then
         accum := v.i;
      else
         error("eval_math", "Can't process " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
      bbs.lisp.memory.deref(el);
      while isList(t) loop
         el := first_value(t);
         if el.kind = E_VALUE then
            v := el.v;
         else
            error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
            return (kind => E_ERROR);
         end if;
         if v.kind /= V_INTEGER then
            error("eval_math", "Can't process " & value_type'Image(v.kind));
            BBS.lisp.memory.deref(el);
            return (kind => E_ERROR);
         end if;
         process_value(v.i, b);
         bbs.lisp.memory.deref(el);
      end loop;
      if t.kind /= E_NIL then
         el := indirect_elem(t);
         if el.kind /= E_VALUE then
            error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
            return (kind => E_ERROR);
         end if;
         v := el.v;
         if v.kind /= V_INTEGER then
            error("eval_math", "Can't process " & value_type'Image(v.kind));
            BBS.lisp.memory.deref(el);
            return (kind => E_ERROR);
         end if;
         print(el, False, True);
         process_value(v.i, b);
      end if;
      return (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum));
   end;
   --
   function add(e : element_type) return element_type is
   begin
      return eval_math(e, PLUS);
   end;
   --
   function sub(e : element_type) return element_type is
   begin
      return eval_math(e, MINUS);
   end;
   --
   function mul(e : element_type) return element_type is
   begin
      return eval_math(e, MUL);
   end;
   --
   function div(e : element_type) return element_type is
   begin
      return eval_math(e, DIV);
   end;
end;
