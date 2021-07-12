with BBS.lisp.memory;
with BBS.lisp.strings;
package body BBS.lisp.evaluate.cond is
   --
   --  Perform comparison operations.
   --
   function eval_comp(s : cons_index; b : compops) return element_type is
      s1 : cons_index := s;
      t1 : element_type;
      t2 : element_type;
      v1 : value;
      v2 : value;
   begin
      if s = cons_index'First then
         error("eval_comp", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      t1 := first_value(s1);
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
      if s1 > NIL_CONS then
         t2 := first_value(s1);
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
            BBS.lisp.strings.deref(v1.s);
            BBS.lisp.strings.deref(v2.s);
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
      elsif ((v1.kind = V_QSYMBOL) or (v1.kind = V_SYMBOL)) and
        ((v2.kind = V_QSYMBOL) or (v2.kind = V_SYMBOL)) then
         declare
            s1 : symb_index;
            s2 : symb_index;
         begin
            if v1.kind = V_QSYMBOL then
               s1 := v1.qsym;
            else
               s1 := v1.sym;
            end if;
            if v2.kind = V_QSYMBOL then
               s2 := v2.qsym;
            else
               s2 := v2.sym;
            end if;
            case b is
            when SYM_EQ =>
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => s1 = s2));
            when SYM_NE =>
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => s1 /= s2));
            when SYM_LT =>
               error("eval_comp", "Can only compare quoted symbols for equality.");
               return (Kind => E_ERROR);
            when SYM_GT =>
               error("eval_comp", "Can only compare quoted symbols for equality.");
               return (Kind => E_ERROR);
            end case;
         end;
      else
         error("eval_comp", "Comparison of the provided types is not supported.");
         put("First type is " & value_type'Image(v1.kind));
         put_line(", second type is " & value_type'Image(v2.kind));
         BBS.lisp.memory.deref(v1);
         BBS.lisp.memory.deref(v2);
         return (kind => E_ERROR);
      end if;
   end;
   --
   --  Compare two items for equality.
   --
   procedure eq(e : out element_type; s : cons_index) is
   begin
      e := eval_comp(s, SYM_EQ);
   end;
   --
   --  Compare two items for not equality.
   --
   procedure ne(e : out element_type; s : cons_index) is
   begin
      e := eval_comp(s, SYM_NE);
   end;
   --
   --  Is first item less than the second item?
   --
   procedure lt(e : out element_type; s : cons_index) is
   begin
      e := eval_comp(s, SYM_LT);
   end;
   --
   --  Is the first item greater than the second item?
   --
   procedure gt(e : out element_type; s : cons_index) is
   begin
      e := eval_comp(s, SYM_GT);
   end;
   --
   --  Perform an IF operation.
   --
   procedure eval_if(e : out element_type; s : cons_index) is
      t  : element_type;
      s1 : cons_index := s;
      p1 : element_type; --  Condition
      p2 : element_type; --  True expression
      p3 : element_type; --  False expression
   begin
      if s = cons_index'First then
         error("eval_if", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = E_ERROR then
         error("eval_if", "Condition reported an error.");
         e := p1;
         return;
      end if;
      if s1 > NIL_CONS then
         p2 := cons_table(s1).car;
         if isList(cons_table(s1).cdr) then
            s1 := getList(cons_table(s1).cdr);
            p3 := cons_table(s1).car;
         else
            p3 := cons_table(s1).cdr;
         end if;
      else
         p2 := cons_table(s).cdr;
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
            t := eval_dispatch(getList(p2));
            if t.kind = E_ERROR then
               error("eval_if", "Error in evaluating true branch");
            end if;
         else
            t := indirect_elem(p2);
         end if;
      else
         if isFunction(p3) then
            t := eval_dispatch(getList(p3));
            if t.kind = E_ERROR then
               error("eval_if", "Error in evaluating false branch");
            end if;
         else
            t := indirect_elem(p3);
         end if;
      end if;
      BBS.lisp.memory.deref(p1);
      e := t;
   end;
end;
