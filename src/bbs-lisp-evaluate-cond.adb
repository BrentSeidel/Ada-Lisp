with BBS.lisp.memory;
with BBS.lisp.strings;
package body BBS.lisp.evaluate.cond is
   --
   --  Perform comparison operations.
   --
   function eval_comp(e : element_type; b : compops) return element_type is
      t  : element_type := e;
      t1 : element_type;
      t2 : element_type;
      v1 : value;
      v2 : value;
   begin
      if e.kind /= E_CONS then
         error("eval_comp", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      t1 := first_value(t);
      if t1.kind = E_ERROR then
         error("eval_comp", "Error reported evaluating first parameter.");
         return t1;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("eval_comp", "First parameter does not evaluate to a value");
         return (kind => E_ERROR);
      end if;
      if isList(t) then
         t2 := first_value(t);
      else
         error("eval_comp", "Cannot compare a single element.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if t2.kind = E_ERROR then
         error("eval_comp", "Error reported evaluating second parameter.");
         BBS.lisp.memory.deref(t1);
         return t2;
      end if;
      if t2.kind = E_VALUE then
         v2 := t2.v;
      else
         error("eval_comp", "Second parameter does not evaluate to a value");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if (v1.kind = V_INTEGER) and (v2.kind = V_INTEGER) then
         case b is
            when SYM_EQ =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.i = v2.i));
            when SYM_NE =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.i /= v2.i));
            when SYM_LT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.i < v2.i));
            when SYM_GT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.i > v2.i));
         end case;
      elsif (v1.kind = V_CHARACTER) and (v2.kind = V_CHARACTER) then
         case b is
            when SYM_EQ =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.c = v2.c));
            when SYM_NE =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.c /= v2.c));
            when SYM_LT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.c < v2.c));
            when SYM_GT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.c > v2.c));
         end case;
      elsif (v1.kind = V_STRING) and (v2.kind = V_STRING) then
         declare
            eq : comparison;
         begin
            eq := bbs.lisp.strings.compare(v1.s, v2.s);
            BBS.lisp.memory.deref(v1.s);
            BBS.lisp.memory.deref(v2.s);
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
      elsif (v1.kind = V_BOOLEAN) and (v2.kind = V_BOOLEAN) then
         case b is
            when SYM_EQ =>
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.b = v2.b));
            when SYM_NE =>
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.b /= v2.b));
            when SYM_LT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.b < v2.b));
            when SYM_GT =>
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => v1.b > v2.b));
         end case;
      else
         error("eval_comp", "Can only compare elements of the same type.");
         put("First type is " & value_type'Image(v1.kind));
         put_line(", second type is " & value_type'Image(v2.kind));
         BBS.lisp.memory.deref(v1);
         BBS.lisp.memory.deref(v2);
         return (kind => E_ERROR);
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
      t : element_type := e;
      p1 : element_type; --  Condition
      p2 : element_type; --  True expression
      p3 : element_type; --  False expression
   begin
      if e.kind /= E_CONS then
         error("eval_if", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("eval_if", "Condition reported an error.");
         return p1;
      end if;
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
      if isTrue(p1) then
         if isFunction(p2) then
            t := eval_dispatch(p2.ps);
            if t.kind = E_ERROR then
               error("eval_if", "Error in evaluating true branch");
            end if;
         else
            t := indirect_elem(p2);
         end if;
      else
         if isFunction(p3) then
            t := eval_dispatch(p3.ps);
            if t.kind = E_ERROR then
               error("eval_if", "Error in evaluating false branch");
            end if;
         else
            t := indirect_elem(p3);
         end if;
      end if;
      BBS.lisp.memory.deref(p1);
      return t;
   end;
end;
