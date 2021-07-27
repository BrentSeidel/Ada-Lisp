with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.symbols;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.loops is
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   procedure dowhile(e : out element_type; s : cons_index) is
      cond : element_type; --  Condition to evaluate
      body_list : element_type; --  List of operations to execute
      t : element_type := NIL_ELEM;
      temp : element_type;
      error_occured : Boolean := False;
   begin
      if s > cons_index'First then
         cond := cons_table(s).car;
         body_list := cons_table(s).cdr;
         --
         --  Loop while the conditions is true.
         --
         if isList(cond) then
            temp := eval_dispatch(getList(cond));
         else
            temp := indirect_elem(cond);
         end if;
         if temp.kind = E_ERROR then
            error("dowhile", "Error occurred during evaluation of condition");
            e := temp;
            return;
         end if;
         while isTrue(temp) and not error_occured loop
            BBS.lisp.memory.deref(t);
            BBS.lisp.memory.deref(temp);
            --
            --  Evaluate all of the items in the list.
            --
            t := execute_block(body_list);
            if t.kind = E_ERROR then
               error("dowhile", "Error occurred during evaluation of body");
               e := t;
               error_occured := True;
            else
               if isList(cond) then
                  temp := eval_dispatch(getList(cond));
               else
                  temp := indirect_elem(cond);
               end if;
               if temp.kind = E_ERROR then
                  error("dowhile", "Error occurred during evaluation of condition");
                  e := temp;
                  error_occured := True;
               end if;
            end if;
            if get_exit_block > 0 then
               decrement_exit_block;
               exit;
            end if;
            exit when error_occured;
         end loop;
         BBS.lisp.memory.deref(temp);
      else
         error("dowhile", "Must provide a condition and expressions.");
         e := (kind => E_ERROR);
         return;
      end if;
      if not error_occured then
         e := t;
      end if;
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
      limits : cons_index; --  Loop parameters to evaluate
      body_list : element_type; --  List of operations to execute
      result : element_type := NIL_ELEM;
      var : element_type := NIL_ELEM;
      rest : cons_index := NIL_CONS;
      limit : element_type := NIL_ELEM;
      limit_value : Natural := 0;
      s1 : cons_index;
      dummy : Natural;
      t : element_type := NIL_ELEM;
      err : Boolean;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 2));
         when PH_PARSE_BEGIN =>
            BBS.lisp.global.stack.start_frame(err);
            if s > NIL_CONS then
               limits := getList(cons_table(s).cdr);
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dotimes", "No parameters provided");
                  e := (kind => E_ERROR);
                  return;
               end if;
               limits := getList(cons_table(limits).car);
               if limits = NIL_CONS then
                  error("dotimes", "List not provided for limits.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               var := cons_table(limits).car;
               rest := getList(cons_table(limits).cdr);
               if isList(var) then
                  error("dotimes", "The loop variable cannot be a list.");
                  BBS.lisp.memory.deref(var);
                  cons_table(limits).car := (Kind => E_ERROR);
                  e := (kind => E_ERROR);
                  return;
               end if;
               if rest = NIL_CONS then
                  error("dotimes", "Loop limit not provided.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               --
               --  Evaluate and validate the loop parameters
               --
               --  First convert the loop variable to a local variable, if not already
               --  done.
               --
               declare
                  str : string_index;
               begin
                  if (var.kind = E_SYMBOL) and then (var.sym.kind = ST_DYNAMIC) then
                     msg("dotimes", "Converting symbol to loop variable");
                     str := BBS.lisp.symbols.get_name(var.sym);
                  elsif (var.kind = E_VALUE) and then (var.v.kind = V_TEMPSYM) then
                     msg("dotimes", "Converting tempsym to loop variable");
                     str := var.v.tempsym;
                  elsif (var.kind = E_VALUE) and then (var.v.kind = V_STACK) then
                     msg("dotimes", "Converting stack variable to loop variable");
                     str := var.v.st_name;
                  else
                     error("dotimes", "Can't convert item into a loop variable.");
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  var := (kind => E_VALUE, v => (kind => V_STACK, st_name => str, st_offset => 1));
                  BBS.lisp.global.stack.push(str, (kind => V_NONE), err);
               end;
               --
               --  Var has been converted to a local variable.  Now put it back into
               --  the list.
               --
               cons_table(limits).car := var;
            end if;
            e := NIL_ELEM;
         when PH_PARSE_END =>
            BBS.lisp.global.stack.exit_frame;
            e := NIL_ELEM;
         when PH_EXECUTE =>
            --
            --  EXECUTE Phase
            --
            if s > NIL_CONS then
               limits := getList(cons_table(s).car);
               body_list := cons_table(s).cdr;
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dotimes", "List not provided for limits.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               var := cons_table(limits).car;
               rest := getList(cons_table(limits).cdr);
               if rest > NIL_CONS then
                  limit := cons_table(rest).car;
                  if isList(limit) then
                     limit := eval_dispatch(getList(limit));
                  else
                     limit := indirect_elem(limit);
                  end if;
                  result := cons_table(rest).cdr;
                  if isList(result) then
                     result := cons_table(getList(result)).car;
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
            if isList(limit) then
               s1 := getList(limit);
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
            if isList(body_list) then
               dummy := BBS.lisp.utilities.replace_sym(getList(body_list), var);
            end if;
            --
            --  Build the stack frame
            --
            if (var.kind = E_VALUE) and then (var.v.kind = V_STACK) then
               BBS.lisp.global.stack.start_frame(err);
               BBS.lisp.global.stack.push(var.v.st_name, (kind => V_INTEGER, i => 0), err);
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
               BBS.lisp.global.stack.set_entry(BBS.lisp.global.stack.get_fp + 1,
                                        (kind => BBS.lisp.stack.ST_VALUE,
                                         st_name => var.v.st_name, st_value =>
                                           (kind => V_INTEGER, i => int32(index))),
                                       err);
               --
               --  Evaluate all of the items in the list.
               --
               BBS.lisp.memory.deref(t);
               t := execute_block(body_list);
               if t.kind = E_ERROR then
                  error("dotimes", "Error occurred in body");
                  BBS.lisp.global.stack.exit_frame;
                  e := t;
                  return;
               end if;
               if get_exit_block > 0 then
                  decrement_exit_block;
                  exit;
               end if;
            end loop;
            BBS.lisp.memory.deref(t);
            --
            --  Exit the stack frame
            --
            BBS.lisp.global.stack.exit_frame;
            if isList(result) then
               t := eval_dispatch(getList(result));
            else
               t := indirect_elem(result);
               if t.kind = E_ERROR then
                  error("dotimes", "Error occurred in body");
               end if;
            end if;
            e := t;
      end case;
   end;
   --
   --  Evaluates a dolist command.  The first item contains up to three elements,
   --  (local list [result]).  "local" is a local variable created for the body
   --  of the loop.  "list" is a list of items to be assigned to local.  The
   --  local variable has values from 0 through count - 1.  "result" is optional.
   --  If present, it is evaluated and returned when the loop completes.  If
   --  absent, the loop returns NIL.  The remaining items are commands that are
   --  executed the specified number of times.
   --  (dotimes (local count [result]) <body>).
   --
   procedure dolist(e : out element_type; s : cons_index; p : phase) is
      limits : cons_index; --  Loop parameters to evaluate
      body_list : element_type; --  List of operations to execute
      result : element_type := NIL_ELEM;
      var : element_type := NIL_ELEM;
      rest : cons_index := NIL_CONS;
      source_list : element_type := NIL_ELEM;
      limit_value : cons_index := NIL_CONS;
      dummy : Natural;
      t : element_type := NIL_ELEM;
      err : Boolean;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 2));
         when PH_PARSE_BEGIN =>
            BBS.lisp.global.stack.start_frame(err);
            if s > NIL_CONS then
               limits := getList(cons_table(s).cdr);
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dolist", "No parameters provided");
                  e := (kind => E_ERROR);
                  return;
               end if;
               limits := getList(cons_table(limits).car);
               if limits = NIL_CONS then
                  error("dolist", "List not provided for limits.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               var := cons_table(limits).car;
               rest := getList(cons_table(limits).cdr);
               if isList(var) then
                  error("dolist", "The loop variable cannot be a list.");
                  BBS.lisp.memory.deref(var);
                  cons_table(limits).car := (Kind => E_ERROR);
                  e := (kind => E_ERROR);
                  return;
               end if;
               if rest = NIL_CONS then
                  error("dolist", "Loop limit not provided.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               --
               --  Evaluate and validate the loop parameters
               --
               --  First convert the loop variable to a local variable, if not already
               --  done.
               --
               declare
                  str : string_index;
               begin
                  if (var.kind = E_SYMBOL) and then (var.sym.kind = ST_DYNAMIC) then
                     msg("dolist", "Converting symbol to loop variable");
                     str := BBS.lisp.symbols.get_name(var.sym);
                  elsif (var.kind = E_VALUE) and then (var.v.kind = V_TEMPSYM) then
                     msg("dolist", "Converting tempsym to loop variable");
                     str := var.v.tempsym;
                  elsif (var.kind = E_VALUE) and then (var.v.kind = V_STACK) then
                     msg("dolist", "Converting stack variable to loop variable");
                     str := var.v.st_name;
                  else
                     error("dolist", "Can't convert item into a loop variable.");
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  var := (kind => E_VALUE, v => (kind => V_STACK, st_name => str, st_offset => 1));
                  BBS.lisp.global.stack.push(str, (kind => V_NONE), err);
               end;
               --
               --  Var has been converted to a local variable.  Now put it back into
               --  the list.
               --
               cons_table(limits).car := var;
            end if;
            e := NIL_ELEM;
         when PH_PARSE_END =>
            BBS.lisp.global.stack.exit_frame;
            e := NIL_ELEM;
         when PH_EXECUTE =>
            --
            --  EXECUTE Phase
            --
            if s > NIL_CONS then
               limits := getList(cons_table(s).car);
               body_list := cons_table(s).cdr;
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dolist", "List not provided for limits.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               var := cons_table(limits).car;
               rest := getList(cons_table(limits).cdr);
               if rest > NIL_CONS then
                  source_list := cons_table(rest).car;
                  if isList(source_list) then
                     source_list := eval_dispatch(getList(source_list));
                  else
                     source_list := indirect_elem(source_list);
                  end if;
                  result := cons_table(rest).cdr;
                  if isList(result) then
                     result := cons_table(getList(result)).car;
                  end if;
               else
                  error("dolist", "Loop limit not provided.");
                  e := (kind => E_ERROR);
                  return;
               end if;
            end if;
            --
            --  Next determine what the loop limit is
            --
            if not isList(source_list) then
               error("dolist", "List not provided for iteration.");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  Find the index variable name in the body and convert all occurences.
            --
            if isList(body_list) then
               dummy := BBS.lisp.utilities.replace_sym(getList(body_list), var);
            end if;
            --
            --  Build the stack frame
            --
            if (var.kind = E_VALUE) and then (var.v.kind = V_STACK) then
               BBS.lisp.global.stack.start_frame(err);
               BBS.lisp.global.stack.push(var.v.st_name, (kind => V_INTEGER, i => 0), err);
            else
               error("dolist", "Loop counter is not a variable");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  Loop with the index variable in the range 0 .. limit.
            --
            limit_value := getList(source_list);
            while limit_value > NIL_CONS loop
               --
               --  Set the value of the local variable on the stack
               --
               BBS.lisp.global.stack.set_entry(BBS.lisp.global.stack.get_fp + 1,
                                        (kind => BBS.lisp.stack.ST_VALUE,
                                         st_name => var.v.st_name, st_value =>
                                           element_to_value(cons_table(limit_value).car)), err);
               --
               --  Evaluate all of the items in the body list.
               --
               BBS.lisp.memory.deref(t);
               t := execute_block(body_list);
               if t.kind = E_ERROR then
                  error("dolist", "Error occurred in body");
                  BBS.lisp.global.stack.exit_frame;
                  e := t;
                  return;
               end if;
               if get_exit_block > 0 then
                  decrement_exit_block;
                  exit;
               end if;
               --
               --  Point to next element in the source list
               --
               limit_value := getList(cons_table(limit_value).cdr);
            end loop;
            BBS.lisp.memory.deref(t);
            --
            --  Exit the stack frame
            --
            BBS.lisp.global.stack.exit_frame;
            if isList(result) then
               t := eval_dispatch(getList(result));
            else
               t := indirect_elem(result);
               if t.kind = E_ERROR then
                  error("dolist", "Error occurred in body");
               end if;
            end if;
            e := t;
      end case;
   end;
   --
   --  Create a block containing multiple statements
   --
   procedure progn(e : out element_type; s : cons_index) is
   begin
      e := execute_block(makeList(s));
   end;
   --
   --  Breaks out of a loop (or other exclosing block) and returns a value
   --
   procedure return_from(e : out element_type; s : cons_index) is
      t : cons_index := s;
   begin
      e := first_value(t);
      BBS.lisp.evaluate.set_exit_block(1);
   end;

end;
