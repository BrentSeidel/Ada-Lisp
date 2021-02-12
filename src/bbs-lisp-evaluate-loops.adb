with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.loops is
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   function dowhile(s : cons_index) return element_type is
      cond : element_type; --  Condition to evaluate
      list : element_type; --  List of operations to execute
      t : element_type := NIL_ELEM;
      temp : element_type;
   begin
      if s > cons_index'First then
         cond := cons_table(s).car;
         list := cons_table(s).cdr;
         --
         --  Loop while the conditions is true.
         --
         if isList(cond) then
            temp := eval_dispatch(getList(cond));
         else
            temp := indirect_elem(cond);
         end if;
         if temp.kind = E_ERROR then
            error("dowhile", "Error occured during evaluation of condition");
            return temp;
         end if;
         while isTrue(temp) loop
            BBS.lisp.memory.deref(t);
            BBS.lisp.memory.deref(temp);
            --
            --  Evaluate all of the items in the list.
            --
            t := execute_block(list);
            if t.kind = E_ERROR then
               error("dowhile", "Error occured during evaluation of body");
               return t;
            end if;
            if isList(cond) then
               temp := eval_dispatch(getList(cond));
            else
               temp := indirect_elem(cond);
            end if;
            if temp.kind = E_ERROR then
               error("dowhile", "Error occured during evaluation of condition");
               return temp;
            end if;
         end loop;
         BBS.lisp.memory.deref(temp);
      else
         error("dowhile", "Must provide a condition and expressions.");
         return (kind => E_ERROR);
      end if;
      return t;
   end;
   --
   --  Evaluates a dotimes command.  The first item contains up to three elements,
   --  (local count [result]).  "local" is a local variable created for the body
   --  of the loop.  "count" is the number of times to execute the body.  The
   --  local variable has values from 0 through count - 1.  "result" is optional.
   --  If present, it is evaluated and returned when the loop completes.  If
   --  absent, the loop returns NIL.  The remaining items are commands that are
   --  executed the specified number of times.
   --  (dotimes (local count [result]) <body>).
   --
   procedure dotimes(e : out element_type; s : cons_index; p : phase) is
      limits : element_type; --  Condition to evaluate
      list : element_type; --  List of operations to execute
      result : element_type := NIL_ELEM;
      var : element_type := NIL_ELEM;
      rest : element_type := NIL_ELEM;
      limit : element_type := NIL_ELEM;
      limit_value : Natural := 0;
      s1 : cons_index;
      dummy : Natural;
      t : element_type := NIL_ELEM;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            return;
         when PH_PARSE_BEGIN =>
            BBS.lisp.stack.start_frame;
            if s > NIL_CONS then
               list := cons_table(s).car;   -- This is the dotimes symbol and ignored here
               limits := cons_table(s).cdr;
               --
               --  Extract local variable, limit, and optional result
               --
               limits := cons_table(limits.ps).car;
               if limits.kind = E_CONS then
                  var := cons_table(limits.ps).car;
                  rest := cons_table(limits.ps).cdr;
               else
                  error("dotimes", "List not provided for limits.");
                  BBS.lisp.stack.enter_frame;
                  e := (kind => E_ERROR);
                  return;
               end if;
               if rest.kind = E_CONS then
                  limit := cons_table(rest.ps).car;
                  if limit.kind = E_CONS then
                     limit := eval_dispatch(limit.ps);
                  else
                     limit := indirect_elem(limit);
                  end if;
                  result := cons_table(rest.ps).cdr;
                  if result.kind = E_CONS then
                     result := cons_table(result.ps).car;
                  end if;
               else
                  error("dotimes", "Loop limit not provided.");
                  BBS.lisp.stack.enter_frame;
                  e := (kind => E_ERROR);
                  return;
               end if;
               --
               --  Evaluate and validate the loop parameters
               --
               --  First convert the loop variable to a local variable, if not already
               --  done.
               --
               if var.kind = E_CONS then
                  error("dotimes", "The loop variable cannot be a list.");
                  BBS.lisp.stack.enter_frame;
                  e := (kind => E_ERROR);
                  return;
               end if;
               declare
                  str : string_index;
               begin
                  if (var.kind = E_SYMBOL) then
                     msg("dotimes", "Converting symbol to loop variable");
                     str := symb_table(var.sym).str;
                  elsif (var.kind = E_TEMPSYM) then
                     msg("dotimes", "Converting tempsym to loop variable");
                     str := var.tempsym;
                  elsif var.kind = E_STACK then
                     msg("dotimes", "Converting stack variable to loop variable");
                     str := var.st_name;
                  else
                     error("dotimes", "Can't convert item into a loop variable.");
                     BBS.lisp.stack.enter_frame;
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  var := (kind => E_STACK, st_name => str, st_offset => 1);
                  BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                       st_name => str,
                                       st_value => (kind => V_NONE)));
               end;
               BBS.lisp.stack.enter_frame;
               --
               --  Var has been converted to a local variable.  Now put it back into
               --  the list.
               --
               cons_table(limits.ps).car := var;
            end if;
            e := NIL_ELEM;
            return;
         when PH_PARSE_END =>
            BBS.lisp.stack.exit_frame;
            e := NIL_ELEM;
            return;
         when PH_EXECUTE =>
            --
            --  EXECUTE Phase
            --
            if s > NIL_CONS then
               limits := cons_table(s).car;
               list := cons_table(s).cdr;
               --
               --  Extract local variable, limit, and optional result
               --
               if limits.kind = E_CONS then
                  var := cons_table(limits.ps).car;
                  rest := cons_table(limits.ps).cdr;
               else
                  error("dotimes", "List not provided for limits.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               if rest.kind = E_CONS then
                  limit := cons_table(rest.ps).car;
                  if limit.kind = E_CONS then
                     limit := eval_dispatch(limit.ps);
                  else
                     limit := indirect_elem(limit);
                  end if;
                  result := cons_table(rest.ps).cdr;
                  if result.kind = E_CONS then
                     result := cons_table(result.ps).car;
                  end if;
               else
                  error("dotimes", "Loop limit not provided.");
                  e := (kind => E_ERROR);
                  return;
               end if;
            end if;
            --
            --  Next determine what the loop limit is
            --
            if limit.kind = E_CONS then
               s1 := limit.ps;
               limit := first_value(s1);
            end if;
            if limit.kind = E_VALUE then
               if limit.v.kind = V_INTEGER then
                  if limit.v.i >= 0 then
                     limit_value := Natural(limit.v.i);
                  else
                     error("dotimes", "Limit must not be negative.");
                     e := (kind => E_ERROR);
                     return;
                  end if;
               else
                  error("dotimes", "Limit is not an integer");
                  e := (kind => E_ERROR);
                  return;
               end if;
            else
               error("dotimes", "Limit is not a value");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  Find the index variable name in the body and convert all occurences.
            --
            if list.kind = E_CONS then
               dummy := BBS.lisp.utilities.replace_sym(list.ps, var);
            end if;
            --
            --  Build the stack frame
            --
            if var.kind = E_STACK then
               BBS.lisp.stack.start_frame;
               BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                    st_name => var.st_name, st_value =>
                                      (kind => V_INTEGER, i => 0)));
               BBS.lisp.stack.enter_frame;
            else
               error("dotimes", "Loop counter is not a variable");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  Loop with the index variable in the range 0 .. limit.
            --
            for index in 0 .. limit_value - 1 loop
               --
               --  Set the value of the local variable on the stack
               --
               BBS.lisp.stack.stack(BBS.lisp.stack.frame_pointer + 1) :=
                 (kind => BBS.lisp.stack.ST_VALUE,
                  st_name => var.st_name, st_value =>
                    (kind => V_INTEGER, i => int32(index)));
               --
               --  Evaluate all of the items in the list.
               --
               BBS.lisp.memory.deref(t);
               t := execute_block(list);
               if t.kind = E_ERROR then
                  error("dotimes", "Error occured in body");
                  BBS.lisp.stack.exit_frame;
                  e := t;
                  return;
               end if;
            end loop;
            BBS.lisp.memory.deref(t);
            --
            --  Exit the stack frame
            --
            BBS.lisp.stack.exit_frame;
            if result.kind = E_CONS then
               t := eval_dispatch(result.ps);
            else
               t := indirect_elem(result);
               if t.kind = E_ERROR then
                  error("dotimes", "Error occured in body");
               end if;
            end if;
            e := t;
            return;
      end case;
   end;

end;
