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
with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.evaluate.loops is
   --
   --  The following routine supports parameters and local variables.
   --  It scans through the passed s expression (recursively, if necessary) and
   --  when it finds a symbol, it looks at the passed in parameter or local
   --  variable.  If the name matches, it replaces the symbol with a pointer to
   --  the parameter or local variable and updates the ref count.  The return
   --  value is the number of replacements made.
   --
   --  Perform the replacement for a single symbol/variable.  Searches the list
   --  s and any symbols or tempsyms whose name matches that of var are replaced
   --  by var.  This means that stack variables will shadow symbols.
   --
   procedure replace_sym(s : cons_index; var : element_type) is
      temp : cons_index := s;
      new_elem : element_type;

      function process_element(e : element_type; var : element_type;
                            replace : out element_type) return Boolean is
         name : string_index;      --  Name of item to potentially replace
         var_name : string_index;  --  Name of potential replacement
         flag : Boolean := False;  --  Was it a tempsym?
      begin
         if e.kind = V_SYMBOL then
            if e.sym.kind = ST_FIXED then
               return False;
            end if;
            name := BBS.lisp.symbols.get_name(e.sym);
         elsif e.kind = V_TEMPSYM then
            name := e.tempsym;
            flag := True;
         else
            return False;
         end if;
         if var.kind = V_STACK then
            var_name := var.st_name;
         else
            error("replace_syms.process_element", "Improper element in library");
         end if;
         if bbs.lisp.strings.compare(name, var_name) = CMP_EQ then
            if flag then
               BBS.lisp.strings.deref(name);
            end if;
            replace := var;
            return True;
         end if;
         return False;
      end;
      --
   begin
      loop
         if BBS.lisp.evaluate.isList(BBS.lisp.conses.get_car(temp)) then
            replace_sym(BBS.lisp.evaluate.getList(BBS.lisp.conses.get_car(temp)), var);
         else
            if process_element(BBS.lisp.conses.get_car(temp), var, new_elem) then
               BBS.lisp.memory.deref(BBS.lisp.conses.get_car(temp));
               BBS.lisp.conses.set_car(temp, new_elem);
               BBS.lisp.memory.ref(BBS.lisp.conses.get_car(temp));
            end if;
         end if;
         exit when not BBS.lisp.evaluate.isList(BBS.lisp.conses.get_cdr(temp));
         temp := BBS.lisp.evaluate.getList(BBS.lisp.conses.get_cdr(temp));
      end loop;
         --
         --  Process the last element, it it exists in a CDR
         --
      if not BBS.lisp.evaluate.isList(BBS.lisp.conses.get_cdr(temp)) then
         if process_element(BBS.lisp.conses.get_cdr(temp), var, new_elem) then
            BBS.lisp.memory.deref(BBS.lisp.conses.get_cdr(temp));
            BBS.lisp.conses.set_cdr(temp, new_elem);
            BBS.lisp.memory.ref(BBS.lisp.conses.get_cdr(temp));
         end if;
      end if;
   end;
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   procedure dowhile(e : out element_type; s : cons_index) is
      cond : element_type; --  Condition to evaluate
      body_list : cons_index; --  List of operations to execute
      t : element_type := NIL_ELEM;
      temp : element_type;
      error_occured : Boolean := False;
   begin
      if s > NIL_CONS then
         cond := BBS.lisp.conses.get_car(s);
         body_list := getList(BBS.lisp.conses.get_cdr(s));
         --
         --  Loop while the conditions is true.
         --
         if isList(cond) then
            temp := eval_dispatch(getList(cond));
         else
            temp := indirect_elem(cond);
         end if;
         if temp.kind = V_ERROR then
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
            if t.kind = V_ERROR then
               error("dowhile", "Error occurred during evaluation of body");
               e := t;
               error_occured := True;
            else
               if isList(cond) then
                  temp := eval_dispatch(getList(cond));
               else
                  temp := indirect_elem(cond);
               end if;
               if temp.kind = V_ERROR then
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
         e := make_error(ERR_NOPARAM);
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
      body_list : cons_index; --  List of operations to execute
      result : element_type := NIL_ELEM;
      var : element_type := NIL_ELEM;
      rest : cons_index := NIL_CONS;
      limit : element_type := NIL_ELEM;
      limit_value : Natural := 0;
      s1 : cons_index;
      t : element_type := NIL_ELEM;
      err : Boolean;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => V_INTEGER, i => 2);
         when PH_PARSE_BEGIN =>
            BBS.lisp.global.stack.start_frame(err);
            if s > NIL_CONS then
               limits := getList(BBS.lisp.conses.get_cdr(s));
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dotimes", "No parameters provided");
                  e := make_error(ERR_NOPARAM);
                  return;
               end if;
               limits := getList(BBS.lisp.conses.get_car(limits));
               if limits = NIL_CONS then
                  error("dotimes", "List not provided for limits.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
               var := BBS.lisp.conses.get_car(limits);
               rest := getList(BBS.lisp.conses.get_cdr(limits));
               if isList(var) then
                  error("dotimes", "The loop variable cannot be a list.");
                  BBS.lisp.memory.deref(var);
                  BBS.lisp.conses.set_car(limits, make_error(ERR_WRONGTYPE));
                  e := make_error(ERR_WRONGTYPE);
                  return;
               end if;
               if rest = NIL_CONS then
                  error("dotimes", "Loop limit not provided.");
                  e := make_error(ERR_FEWPARAM);
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
                  if (var.kind = V_SYMBOL) and then (var.sym.kind = ST_DYNAMIC) then
                     msg("dotimes", "Converting symbol to loop variable");
                     str := BBS.lisp.symbols.get_name(var.sym);
                  elsif var.kind = V_TEMPSYM then
                     msg("dotimes", "Converting tempsym to loop variable");
                     str := var.tempsym;
                  elsif var.kind = V_STACK then
                     msg("dotimes", "Converting stack variable to loop variable");
                     str := var.st_name;
                  else
                     error("dotimes", "Can't convert item into a loop variable.");
                     e := make_error(ERR_WRONGTYPE);
                     return;
                  end if;
                  var := (kind => V_STACK, st_name => str, st_offset => 1);
                  BBS.lisp.global.stack.push(str, (kind => V_NONE), err);
               end;
               --
               --  Var has been converted to a local variable.  Now put it back into
               --  the list.
               --
               BBS.lisp.conses.set_car(limits, var);
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
               limits := getList(BBS.lisp.conses.get_car(s));
               body_list := getList(BBS.lisp.conses.get_cdr(s));
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dotimes", "List not provided for limits.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
               var := BBS.lisp.conses.get_car(limits);
               rest := getList(BBS.lisp.conses.get_cdr(limits));
               if rest > NIL_CONS then
                  limit := BBS.lisp.conses.get_car(rest);
                  if isList(limit) then
                     limit := eval_dispatch(getList(limit));
                  else
                     limit := indirect_elem(limit);
                  end if;
                  result := BBS.lisp.conses.get_cdr(rest);
                  if isList(result) then
                     result := BBS.lisp.conses.get_car(getList(result));
                  end if;
               else
                  error("dotimes", "Loop limit not provided.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
            else
               error("dotimes", "List not provided for limits.");
               e := make_error(ERR_NOPARAM);
               return;
            end if;
            --
            --  Next determine what the loop limit is
            --
            if isList(limit) then
               s1 := getList(limit);
               limit := first_value(s1);
            end if;
            if limit.kind = V_INTEGER then
               if limit.i >= 0 then
                  limit_value := Natural(limit.i);
               else
                  error("dotimes", "Limit must not be negative.");
                  e := make_error(ERR_RANGE);
                  return;
               end if;
            else
               error("dotimes", "Limit is not an integer");
               e := make_error(ERR_WRONGTYPE);
               return;
            end if;
            --
            --  Find the index variable name in the body and convert all occurences.
            --
            if body_list /= NIL_CONS then
               replace_sym(body_list, var);
            end if;
            --
            --  Build the stack frame
            --
            if var.kind = V_STACK then
               BBS.lisp.global.stack.start_frame(err);
               BBS.lisp.global.stack.push(var.st_name, (kind => V_INTEGER, i => 0), err);
            else
               error("dotimes", "Loop counter is not a variable");
               e := make_error(ERR_WRONGTYPE);
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
                                         st_name => var.st_name, st_value =>
                                           (kind => V_INTEGER, i => int32(index))),
                                       err);
               --
               --  Evaluate all of the items in the list.
               --
               BBS.lisp.memory.deref(t);
               t := execute_block(body_list);
               if t.kind = V_ERROR then
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
               if t.kind = V_ERROR then
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
      body_list : cons_index; --  List of operations to execute
      result : element_type := NIL_ELEM;
      var : element_type := NIL_ELEM;
      rest : cons_index := NIL_CONS;
      source_list : element_type := NIL_ELEM;
      limit_value : cons_index := NIL_CONS;
      t : element_type := NIL_ELEM;
      err : Boolean;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => V_INTEGER, i => 2);
         when PH_PARSE_BEGIN =>
            BBS.lisp.global.stack.start_frame(err);
            if s > NIL_CONS then
               limits := getList(BBS.lisp.conses.get_cdr(s));
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dolist", "No parameters provided");
                  e := make_error(ERR_NOPARAM);
                  return;
               end if;
               limits := getList(BBS.lisp.conses.get_car(limits));
               if limits = NIL_CONS then
                  error("dolist", "List not provided for limits.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
               var := BBS.lisp.conses.get_car(limits);
               rest := getList(BBS.lisp.conses.get_cdr(limits));
               if isList(var) then
                  error("dolist", "The loop variable cannot be a list.");
                  BBS.lisp.memory.deref(var);
                  BBS.lisp.conses.set_car(limits, make_error(ERR_WRONGTYPE));
                  e := make_error(ERR_WRONGTYPE);
                  return;
               end if;
               if rest = NIL_CONS then
                  error("dolist", "Loop limit not provided.");
                  e := make_error(ERR_FEWPARAM);
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
                  if (var.kind = V_SYMBOL) and then (var.sym.kind = ST_DYNAMIC) then
                     msg("dolist", "Converting symbol to loop variable");
                     str := BBS.lisp.symbols.get_name(var.sym);
                  elsif var.kind = V_TEMPSYM then
                     msg("dolist", "Converting tempsym to loop variable");
                     str := var.tempsym;
                  elsif var.kind = V_STACK then
                     msg("dolist", "Converting stack variable to loop variable");
                     str := var.st_name;
                  else
                     error("dolist", "Can't convert item into a loop variable.");
                     e := make_error(ERR_WRONGTYPE);
                     return;
                  end if;
                  var := (kind => V_STACK, st_name => str, st_offset => 1);
                  BBS.lisp.global.stack.push(str, (kind => V_NONE), err);
               end;
               --
               --  Var has been converted to a local variable.  Now put it back into
               --  the list.
               --
               BBS.lisp.conses.set_car(limits, var);
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
               limits := getList(BBS.lisp.conses.get_car(s));
               body_list := getList(BBS.lisp.conses.get_cdr(s));
               --
               --  Extract local variable, limit, and optional result
               --
               if limits = NIL_CONS then
                  error("dolist", "List not provided for limits.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
               var := BBS.lisp.conses.get_car(limits);
               rest := getList(BBS.lisp.conses.get_cdr(limits));
               if rest > NIL_CONS then
                  source_list := BBS.lisp.conses.get_car(rest);
                  if isList(source_list) then
                     source_list := eval_dispatch(getList(source_list));
                  else
                     source_list := indirect_elem(source_list);
                  end if;
                  result := BBS.lisp.conses.get_cdr(rest);
                  if isList(result) then
                     result := BBS.lisp.conses.get_car(getList(result));
                  end if;
               else
                  error("dolist", "Loop limit not provided.");
                  e := make_error(ERR_FEWPARAM);
                  return;
               end if;
            else
               error("dolist", "List not provided for limits.");
               e := make_error(ERR_NOPARAM);
               return;
            end if;
            --
            --  Next determine what the loop limit is
            --
            if not isList(source_list) then
               error("dolist", "List not provided for iteration.");
               e := make_error(ERR_FEWPARAM);
               return;
            end if;
            --
            --  Find the index variable name in the body and convert all occurences.
            --
            if body_list /= NIL_CONS then
               replace_sym(body_list, var);
            end if;
            --
            --  Build the stack frame
            --
            if var.kind = V_STACK then
               BBS.lisp.global.stack.start_frame(err);
               BBS.lisp.global.stack.push(var.st_name, (kind => V_INTEGER, i => 0), err);
            else
               error("dolist", "Loop counter is not a variable");
               e := make_error(ERR_WRONGTYPE);
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
                                         st_name => var.st_name, st_value =>
                                           BBS.lisp.evaluate.indirect_elem(BBS.lisp.conses.get_car(limit_value))), err);
               --
               --  Evaluate all of the items in the body list.
               --
               BBS.lisp.memory.deref(t);
               t := execute_block(body_list);
               if t.kind = V_ERROR then
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
               limit_value := getList(BBS.lisp.conses.get_cdr(limit_value));
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
               if t.kind = V_ERROR then
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
      e := execute_block(s);
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
