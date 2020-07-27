with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.utilities;
--with BBS.lisp.stack;
package body BBS.lisp.evaluate.cond is
   --
   --  Perform comparison operations.
   --
   function eval_comp(e : element_type; b : compops) return element_type is
      t  : element_type;
      t1 : element_type;
      t2 : element_type;
      v1 : value;
      v2 : value;
      i1 : Integer;
      i2 : Integer;
   begin
      if e.kind = E_CONS then
         BBS.lisp.utilities.first_value(e, t1, t);
         if t.kind = E_CONS then
            BBS.lisp.utilities.first_value(t, t2, t);
         else
            error("eval_comp", "Cannot compare a single element.");
            return NIL_ELEM;
         end if;
      else
         error("eval_comp", "Cannot compare a single element.");
         return NIL_ELEM;
      end if;
      if (t1.kind /= E_CONS) and (t2.kind /= E_CONS) then
         t := bbs.lisp.utilities.indirect_elem(t1);
         if t.kind = E_VALUE then
            v1 := t.v;
         end if;
         t := bbs.lisp.utilities.indirect_elem(t2);
         if t.kind = E_VALUE then
            v2 := t.v;
         end if;
         if v1.kind = V_INTEGER and
           v2.kind = V_INTEGER then
            i1 := v1.i;
            i2 := v2.i;
            case b is
               when SYM_EQ =>
                  if i1 = i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_NE =>
                  if i1 /= i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_LT =>
                  if i1 < i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_GT =>
                  if i1 > i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               end case;
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         elsif v1.kind = V_STRING and
           v2.kind = V_STRING then
            declare
               eq : comparison;
            begin
               eq := bbs.lisp.strings.compare(v1.s, v2.s);
               case b is
                  when SYM_EQ =>
                     if eq = CMP_EQ then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_LT =>
                     if eq = CMP_LT then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_GT =>
                     if eq = CMP_GT then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_NE =>
                     if eq /= CMP_EQ then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
               end case;
            end;
            return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         else
            error("eval_comp", "Can only compare integers, strings, or symbols.");
            put("First type is " & value_type'Image(v1.kind));
            put_line(", second type is " & value_type'Image(v2.kind));
            return NIL_ELEM;
         end if;
      else
         error("eval_comp", "Can only compare two elements.");
         put("First type is " & ptr_type'Image(t1.kind));
         put_line(", second type is " & ptr_type'Image(t2.kind));
         return NIL_ELEM;
      end if;
   end;
   --
   function eq(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_EQ);
   end;
   --
   function ne(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_NE);
   end;
   --
   function lt(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_LT);
   end;
   --
   function gt(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_GT);
   end;
   --
   function eval_if(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Condition
      p2 : element_type; --  True expression
      p3 : element_type; --  False expression
   begin
      if e.kind /= E_CONS then
         error("eval_if", "Internal error.  Should have a list.");
         return NIL_ELEM;
      end if;
      BBS.lisp.utilities.first_value(e, p1, t);
      if t.kind = E_CONS then
         p2 := cons_table(t.ps).car;
         t := cons_table(t.ps).cdr;
         if t.kind = E_CONS then
            p3 := cons_table(t.ps).car;
         else
            p3 := t;
         end if;
      else
         p2 := t;
         p3 := NIL_ELEM;
      end if;
      --
      --  Now p1 contains the results of evaluating the condition, p2 the
      --  "then" branch, and p3 the "else" branch.  Decide which of p2 or p3
      --  to evaluate.
      --
      t := NIL_ELEM;
      if BBS.lisp.utilities.isTrue(p1) then
         if BBS.lisp.utilities.isFunction(p2) then
            t := eval_dispatch(p2.ps);
         else
            t := BBS.lisp.utilities.indirect_elem(p2);
         end if;
      else
         if BBS.lisp.utilities.isFunction(p3) then
            t := eval_dispatch(p3.ps);
         else
            t := BBS.lisp.utilities.indirect_elem(p3);
         end if;
      end if;
      BBS.lisp.memory.deref(p1);
      return t;
   end;
end;
