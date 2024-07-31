--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of Tiny-Lisp.
--  Tiny-Lisp is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  Tiny-Lisp is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS.lisp.conses;
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
   begin
      if s = cons_index'First then
         error("eval_comp", "No parameters provided.");
         return make_error(ERR_NOPARAM);
      end if;
      t1 := first_value(s1);
      if s1 > NIL_CONS then
         t2 := first_value(s1);
      else
         error("eval_comp", "Cannot compare a single element.");
         BBS.lisp.memory.deref(t1);
         return make_error(ERR_FEWPARAM);
      end if;
      --
      --  A value of V_NONE is equivalent to a boolean False.
      --
      if t1.kind = V_NONE then
         t1 := ELEM_F;
      end if;
      if t2.kind = V_NONE then
         t2 := ELEM_F;
      end if;
      --
      --  Integer comparison
      --
      if (t1.kind = V_INTEGER) and (t2.kind = V_INTEGER) then
         case b is
            when SYM_EQ =>
               return (kind => V_BOOLEAN, b => t1.i = t2.i);
            when SYM_NE =>
               return (kind => V_BOOLEAN, b => t1.i /= t2.i);
            when SYM_LT =>
               return (kind => V_BOOLEAN, b => t1.i < t2.i);
            when SYM_GT =>
               return (kind => V_BOOLEAN, b => t1.i > t2.i);
         end case;
         --
         --  Character comparison
         --
      elsif (t1.kind = V_CHARACTER) and (t2.kind = V_CHARACTER) then
         case b is
            when SYM_EQ =>
               return (kind => V_BOOLEAN, b => t1.c = t2.c);
            when SYM_NE =>
               return (kind => V_BOOLEAN, b => t1.c /= t2.c);
            when SYM_LT =>
               return (kind => V_BOOLEAN, b => t1.c < t2.c);
            when SYM_GT =>
               return (kind => V_BOOLEAN, b => t1.c > t2.c);
         end case;
         --
         --  String comparison
         --
      elsif (t1.kind = V_STRING) and (t2.kind = V_STRING) then
         declare
            eq : comparison;
         begin
            eq := BBS.lisp.strings.compare(t1.s, t2.s);
            BBS.lisp.strings.deref(t1.s);
            BBS.lisp.strings.deref(t2.s);
            case b is
               when SYM_EQ =>
                  if eq = CMP_EQ then
                     return (kind => V_BOOLEAN, b => True);
                  end if;
               when SYM_LT =>
                  if eq = CMP_LT then
                     return (kind => V_BOOLEAN, b => True);
                  end if;
               when SYM_GT =>
                  if eq = CMP_GT then
                     return (kind => V_BOOLEAN, b => True);
                  end if;
               when SYM_NE =>
                  if eq /= CMP_EQ then
                     return (kind => V_BOOLEAN, b => True);
                  end if;
            end case;
         end;
         return (kind => V_BOOLEAN, b => False);
         --
         --  Boolean comparison
         --
      elsif (t1.kind = V_BOOLEAN) and (t2.kind = V_BOOLEAN) then
         case b is
            when SYM_EQ =>
               return (kind => V_BOOLEAN, b => t1.b = t2.b);
            when SYM_NE =>
               return (kind => V_BOOLEAN, b => t1.b /= t2.b);
            when SYM_LT =>
               return (kind => V_BOOLEAN, b => t1.b < t2.b);
            when SYM_GT =>
               return (kind => V_BOOLEAN, b => t1.b > t2.b);
         end case;
         --
         --  Symbol comparison
         --
      elsif ((t1.kind = V_QSYMBOL) or (t1.kind = V_SYMBOL)) and
        ((t2.kind = V_QSYMBOL) or (t2.kind = V_SYMBOL)) then
         declare
            s1 : symbol_ptr;
            s2 : symbol_ptr;
         begin
            if t1.kind = V_QSYMBOL then
               s1 := t1.qsym;
            else
               s1 := t1.sym;
            end if;
            if t2.kind = V_QSYMBOL then
               s2 := t2.qsym;
            else
               s2 := t2.sym;
            end if;
            case b is
            when SYM_EQ =>
               return (kind => V_BOOLEAN, b => s1 = s2);
            when SYM_NE =>
               return (kind => V_BOOLEAN, b => s1 /= s2);
            when SYM_LT =>
               error("eval_comp", "Can only compare symbols for equality.");
               return make_error(ERR_WRONGTYPE);
            when SYM_GT =>
               error("eval_comp", "Can only compare symbols for equality.");
               return make_error(ERR_WRONGTYPE);
            end case;
         end;
         --
         --  Compare errors for equality.
         --
      elsif (t1.kind = V_ERROR) and (t2.kind = V_ERROR) then
         case b is
            when SYM_EQ =>
               return (kind => V_BOOLEAN, b => t1.err = t2.err);
            when SYM_NE =>
               return (kind => V_BOOLEAN, b => t1.err /= t2.err);
            when others =>
               error("eval_comp", "Can only compare errors for equality.");
               return make_error(ERR_WRONGTYPE);
         end case;
      else
         --
         --  Other comparisons are not supported.
         --
         error("eval_comp", "Comparison of the provided types is not supported.");
         put("First type is " & value_type'Image(t1.kind));
         put_line(", second type is " & value_type'Image(t2.kind));
         BBS.lisp.memory.deref(t1);
         BBS.lisp.memory.deref(t2);
         return make_error(ERR_WRONGTYPE);
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
      if s = NIL_CONS then
         error("if", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p1 := first_value(s1);
      if p1.kind = V_ERROR then
         error("if", "Condition reported an error.");
         e := p1;
         return;
      end if;
      if s1 > NIL_CONS then
         p2 := BBS.lisp.conses.get_car(s1);
         if isList(BBS.lisp.conses.get_cdr(s1)) then
            s1 := getList((BBS.lisp.conses.get_cdr(s1)));
            p3 := BBS.lisp.conses.get_car(s1);
         else
            p3 := BBS.lisp.conses.get_cdr(s1);
         end if;
      else
         p2 := BBS.lisp.conses.get_cdr(s);
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
            if t.kind = V_ERROR then
               error("if", "Error in evaluating true branch");
            end if;
         else
            t := indirect_elem(p2);
         end if;
      else
         if isFunction(p3) then
            t := eval_dispatch(getList(p3));
            if t.kind = V_ERROR then
               error("if", "Error in evaluating false branch");
            end if;
         else
            t := indirect_elem(p3);
         end if;
      end if;
      BBS.lisp.memory.deref(p1);
      e := t;
   end;
   --
   --  Perform a COND operation.
   --
   procedure eval_cond(e : out element_type; s : cons_index) is
      s1 : cons_index := s;  -- Walks through the parameter list
      s2 : cons_index;   --  Walks through each candidate
      p1 : element_type := NIL_ELEM; --  Candidate
      test : element_type;  -- Condition value
   begin
      if s = NIL_CONS then
         error("cond", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Loop for each element of parameter list
      --
      loop
         BBS.lisp.memory.deref(p1);
         p1 := first_value(s1);  -- Get the next candidate
         if p1.kind = V_ERROR then
            error("cond", "Candidate reported an error.");
            e := p1;
            return;
         end if;
         s2 := getList(p1);  -- Check if it is a list
         if s2 = NIL_CONS then
            error("cond", "Each candidate branch must be a list.");
            e := make_error(ERR_WRONGTYPE);
            BBS.lisp.memory.deref(p1);
            return;
         end if;
         test := first_value(s2);  -- Check if the condition is true
         if test.kind = V_ERROR then
            error("cond", "Condition reported an error.");
            e := test;
            return;
         end if;
         if isTrue(test) then
            e := execute_block(s2);
            BBS.lisp.memory.deref(test);
            BBS.lisp.memory.deref(p1);
            return;
         end if;
         if s1 = NIL_CONS then
            error("cond", "No matching candidate found.");
            e := make_error(ERR_FEWPARAM);
            BBS.lisp.memory.deref(p1);
            return;
         end if;
      end loop;
   end;
end;
