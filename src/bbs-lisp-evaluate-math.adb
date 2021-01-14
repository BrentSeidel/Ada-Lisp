with BBS.lisp.memory;

package body BBS.lisp.evaluate.math is
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(s : cons_index; b : mathops) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
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
      if s = cons_index'First then
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
   function add(s : cons_index) return element_type is
   begin
      return eval_math(s, PLUS);
   end;
   --
   function sub(s : cons_index) return element_type is
   begin
      return eval_math(s, MINUS);
   end;
   --
   function mul(s : cons_index) return element_type is
   begin
      return eval_math(s, MUL);
   end;
   --
   function div(s : cons_index) return element_type is
   begin
      return eval_math(s, DIV);
   end;
end;
