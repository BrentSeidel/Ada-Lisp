with BBS.lisp.memory;
--with BBS.lisp.strings;
--with BBS.lisp.stack;
with BBS.lisp.utilities;

package body BBS.lisp.evaluate.math is
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(e : element_type; b : mathops) return element_type is
      accum : Integer := 0;
      v : value;
      p : cons_index;
      el: element_type;
      temp : element_type;
      --
      --  Subfunction to do the actual evaluation.
      --
      function process_value(e : element_type; accum : Integer; b : mathops) return Integer is
         v  : value;
         e1 : element_type;
      begin
         e1 := bbs.lisp.utilities.indirect_elem(e);
         if e1.kind = E_VALUE then
            v := e.v;
            if v.kind = V_INTEGER then
               case (b) is
               when PLUS =>
                  return accum + v.i;
               when MUL =>
                  return accum * v.i;
               when MINUS =>
                  return accum - v.i;
               when DIV =>
                  return accum / v.i;
--               when others =>
--                  error("eval_math.process_atom", "Internal error, unknown math operation");
               end case;
            else
               error("eval_math.process_atom", "Can't process " & value_type'Image(v.kind));
            end if;
         else
            error("eval_math.process_atom", "Can't process " & ptr_type'Image(e1.kind));
         end if;
         return accum;
      end;

   begin
      if e.kind = E_VALUE then
         if e.v.kind = V_INTEGER then
            accum := e.v.i;
         end if;
      elsif e.kind = E_CONS then
         p := e.ps;
         if cons_table(p).car.kind /= E_CONS then
            el := BBS.lisp.utilities.indirect_elem(cons_table(p).car);
            if el.kind = E_VALUE then
               v := el.v;
            else
              error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
            end if;
            if v.kind = V_INTEGER then
               accum := v.i;
            else
               error("eval_math", "Can't process " & value_type'Image(v.kind));
            end if;
         elsif cons_table(p).car.kind = E_CONS then
            temp := eval_dispatch(cons_table(p).car.ps);
            if temp.kind /= E_CONS then
               el := bbs.lisp.utilities.indirect_elem(temp);
               if el.kind = E_VALUE then
                  v := el.v;
               end if;
               if v.kind = V_INTEGER then
                  accum := v.i;
               else
                  error("eval_math", "Can't process " & value_type'Image(v.kind));
               end if;
            end if;
            bbs.lisp.memory.deref(temp);
         end if;
         if cons_table(p).cdr.kind /= E_NIL then
            p := cons_table(p).cdr.ps;
            loop
               if cons_table(p).car.kind = E_CONS then
                  temp := eval_dispatch(cons_table(p).car.ps);
                  if temp.kind = E_VALUE then
                     accum := process_value(temp, accum, b);
                  end if;
                  bbs.lisp.memory.deref(temp);
               else
                  el := bbs.lisp.utilities.indirect_elem(cons_table(p).car);
                  accum := process_value(el, accum, b);
               end if;
               exit when cons_table(p).cdr.kind /= E_CONS;
               p := cons_table(p).cdr.ps;
            end loop;
            if cons_table(p).cdr.kind /= E_NIL then
               el := bbs.lisp.utilities.indirect_elem(cons_table(p).cdr);
               accum := process_value(el, accum, b);
            end if;
         end if;
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
