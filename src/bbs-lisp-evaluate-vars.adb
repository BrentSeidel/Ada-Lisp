with BBS.lisp.conses;
with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.symbols;
package body BBS.lisp.evaluate.vars is
   --
   --  This sets a symbol or stack variable to a value.  The first parameter
   --  must evaluate to a symbol, stack variable, or temp symbol.  If it is a
   --  temp symbol, it is converted to a perminant symbol in the symbol table.
   --  The assigned value is the result of evaluating the second parameter.
   --
   procedure setq(e : out element_type; s : cons_index; p : phase) is
      symb : symbol_ptr;
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
      temp : cons_index;
      str : string_index;
      stacked : Boolean := False;
      index : Natural;
      err : Boolean := False;

   begin
      case p is
         when PH_QUERY =>
            e := (kind => V_INTEGER, i => 2);
            return;
         when PH_PARSE_BEGIN =>
            --
            --  PH_PARSE_BEGIN is only called after some parameters have been
            --  parsed, so we can assume that s is actually pointing to a list.
            --
            msg("setq", "Called during parse begin phase.");
            p1 := BBS.lisp.conses.get_car(s);  --  Should be symbol for setq
            p2 := BBS.lisp.conses.get_cdr(s);
            p3 := BBS.lisp.conses.get_car(getList(p2)); --  Should be a symbol or tempsym
            if p3.kind = V_SYMBOL then
               symb := p3.sym;
               if BBS.lisp.symbols.isFixed(symb) then
                  error("setq", "Can't assign a value to a builtin or special symbol");
                  e := make_error(ERR_FIXSYM);
                  return;
               end if;
            elsif p3.kind = V_TEMPSYM then
               str := p3.tempsym;
               p3 := find_variable(str, True);
               BBS.lisp.strings.deref(str);
               BBS.lisp.conses.set_car(getList(p2), p3);
            elsif p3.kind = V_STACK then
               null;
            else
               error("setq", "First parameter is not a symbol or temporary symbol.");
               Put_Line("Parameter type is " & value_type'Image(p3.kind));
               e := make_error(ERR_WRONGTYPE);
               return;
            end if;
         when PH_PARSE_END =>
            null;
         when PH_EXECUTE =>
            msg("setq", "Called during execute phase.");
            if s > NIL_CONS then
               p1 := BBS.lisp.conses.get_car(s);  --  Should be symbol name
               if p1.kind = V_SYMBOL then
                  symb := p1.sym;
               elsif p1.kind = V_STACK then
                  stacked := True;
               else
                  error("setq", "First parameter is not a symbol.");
                  Put_Line("Kind is " & value_type'Image(p1.kind));
                  e := make_error(ERR_NOTSYM);
                  return;
               end if;
               if not stacked then
                  if BBS.lisp.symbols.isFixed(symb) then
                     error("setq", "Can't assign a value to a builtin or special symbol");
                     e := make_error(ERR_FIXSYM);
                     return;
                  end if;
               end if;
               --
               --  At this point, p1 should be an element containing a valid symbol and
               --  symb is the index to that symbol.
               --
               --
               --  Now determine what value to attach to the symbol.
               --
               if isList(BBS.lisp.conses.get_cdr(s)) then
                  temp := getList(BBS.lisp.conses.get_cdr(s));
                  p2 := first_value(temp);
               else
                  p2 := BBS.lisp.conses.get_cdr(s);
               end if;
               BBS.lisp.memory.ref(p2);
                  --
                  --  Check for stack variables
                  --
               if stacked then
                  if p1.kind = V_STACK then
                     index := BBS.lisp.global.stack.search_frames(p1.st_offset, p1.st_name);
                     BBS.lisp.memory.deref(BBS.lisp.global.stack.get_entry(index, err).st_value);
                     if isList(p2) then
                        BBS.lisp.global.stack.set_value(index, (kind => V_LIST, l => getList(p2)), err);
                     else
                        BBS.lisp.global.stack.set_value(index, p2, err);
                     end if;
                     if err then
                        error("setq", "Error occured setting stack variable");
                        e := make_error(ERR_STACK);
                        return;
                     end if;
                  end if;
               else
                  if BBS.lisp.symbols.get_type(symb) = SY_VARIABLE then
                     BBS.lisp.memory.deref(BBS.lisp.symbols.get_value(symb));
                  end if;
                  BBS.lisp.symbols.set_sym(symb, (Kind => SY_VARIABLE, pv => p2));
               end if;
               e := p2;
            else
               error("setq", "No parameters provided.");
               e := make_error(ERR_NOPARAM);
            end if;
            return;
      end case;
      e := NIL_ELEM;
   end;
   --
   --  Define local variables and optionally assign values to them.
   --
   procedure local(e : out element_type; s : cons_index; p : phase) is
      locals : cons_index;
      base : cons_index;
      list : cons_index;
      t : element_type := NIL_ELEM;
      err : Boolean;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => V_INTEGER, i => 2);
            return;
         when PH_PARSE_BEGIN =>
            --
            --  PH_PARSE_BEGIN is only called after some parameters have been
            --  parsed, so we can assume that s is actually pointing to a list.
            --
            BBS.lisp.global.stack.start_frame(err);
            if err then
               error("let", "Error creating stack frame during parsing");
               e := make_error(ERR_STACK);
            end if;
            --
            --  First process the list of local variables
            --
            list := getList(BBS.lisp.conses.get_cdr(s));  --  Should be local variable list.
            if list /= NIL_CONS then
               locals := getList(BBS.lisp.conses.get_car(list));
            else
               error("let", "Improper parameters.");
               e := make_error(ERR_WRONGTYPE);
               return;
            end if;
            if locals = NIL_CONS then
               error("let", "Parameter list must be a list");
               BBS.lisp.memory.deref(BBS.lisp.conses.get_car(list));
               BBS.lisp.conses.set_car(list, make_error(ERR_WRONGTYPE));
               e := make_error(ERR_WRONGTYPE);
               return;
            end if;
            base := locals;
            while locals /= NIL_CONS loop
               --
               --  If the local variable is a cons, then the first element
               --  is the variable name and the second element is the value.
               --  At this point, the value is ignored.
               --
               declare
                  el : element_type;
                  str : string_index;
                  offset : Natural := 1;
               begin
                  if isList(BBS.lisp.conses.get_car(locals)) then
                     el := BBS.lisp.conses.get_car(getList(BBS.lisp.conses.get_car(locals)));
                  else
                     el := BBS.lisp.conses.get_car(locals);
                  end if;
                  if (el.kind = V_SYMBOL) and then (el.sym.kind = ST_DYNAMIC) then
                     str := BBS.lisp.symbols.get_name(el.sym);
                     msg("let", "Converting symbol to local variable");
                  elsif el.kind = V_TEMPSYM then
                     str := el.tempsym;
                     msg("let", "Converting tempsym to local variable");
                  elsif el.kind = V_STACK then
                     msg("let", "Converting stack variable to local variable");
                     str := el.st_name;
                  else
                     error("let", "Can't convert item into a local variable.");
                     print(el, False, True);
                     Put_Line("Item is of kind " & value_type'Image(el.kind));
                     e := make_error(ERR_WRONGTYPE);
                     return;
                  end if;
                  el := (kind => V_STACK, st_name => str, st_offset => offset);
                  BBS.lisp.global.stack.push(str, (kind => V_NONE), err);
                  if err then
                     error("let", "Error building stack frame during parsing");
                     put_line("Local variable list removed");
                     BBS.lisp.conses.deref(base);
                     e := make_error(ERR_STACK);
                     BBS.lisp.conses.set_car(list, make_error(ERR_STACK));
                     return;
                  end if;
                  offset := offset + 1;
                  if isList(BBS.lisp.conses.get_car(locals)) then
                     BBS.lisp.conses.set_car(getList(BBS.lisp.conses.get_car(locals)), el);
                  else
                     BBS.lisp.conses.set_car(locals, el);
                  end if;
               end;
               locals := getList(BBS.lisp.conses.get_cdr(locals));
            end loop;
         when PH_PARSE_END =>
            BBS.lisp.global.stack.exit_frame;
         when PH_EXECUTE =>
            if s = NIL_CONS then
               error("let", "No parameters provided.");
               e := make_error(ERR_NOPARAM);
               return;
            end if;
            --
            --  First process the list of local variables
            --
            locals := getList(BBS.lisp.conses.get_car(s));  --  Should be parameter list.
            list := getList(BBS.lisp.conses.get_cdr(s));
            --
            --  Next process the parameter list.
            --
            if locals = NIL_CONS then
               error("let", "No list of local variables");
               e := make_error(ERR_WRONGTYPE);
               return;
            end if;
            BBS.lisp.global.stack.start_frame(err);
            while locals > NIL_CONS loop
               --
               --  If the local variable is a cons, then the first element
               --  is the variable name and the second element is the value.
               --
               declare
                  el : element_type;
                  temp_list : cons_index;
                  check : element_type;
                  offset : Natural := 1;
                  local_val : element_type := ELEM_F;
               begin
                  if isList(BBS.lisp.conses.get_car(locals)) then
                     temp_list := getList(BBS.lisp.conses.get_car(locals));
                     el := BBS.lisp.conses.get_car(temp_list);
                     --
                     -- Check if there is a value
                     --
                     check := BBS.lisp.conses.get_cdr(temp_list);
                     if isList(check) then
                        check := BBS.lisp.conses.get_car(getList(check));
                     end if;
                     if isList(check) then
                        check := eval_dispatch(getList(check));
                     else
                        check := indirect_elem(check);
                     end if;
                     if isList(check) then
                        local_val := (kind => V_LIST, l => getList(check));
                     else
                        local_val := check;
                     end if;
                  else
                     el := BBS.lisp.conses.get_car(locals);
                  end if;
                  if el.kind = V_STACK then
                     BBS.lisp.global.stack.push(el.st_name, local_val, err);
                     if err then
                        error("let", "Error building stack frame during execution");
                        BBS.lisp.global.stack.exit_frame;
                        e := make_error(ERR_STACK);
                        return;
                     end if;
                  else
                     error("let", "Local variable is not a local");
                     put("Item is: ");
                     print(el, False, False);
                     Put_Line(", Item is of kind " & value_type'Image(el.kind));
                     BBS.lisp.global.stack.exit_frame;
                     e := make_error(ERR_STACK);
                     return;
                  end if;
                  offset := offset + 1;
               end;
               locals := getList(BBS.lisp.conses.get_cdr(locals));
            end loop;
            --
            --  Now evaluate the statements in this context.
            --
            t := execute_block(list);
            if t.kind = V_ERROR then
               error("let", "Error occured evaluating statement");
            end if;
            BBS.lisp.global.stack.exit_frame;
            e := t;
            return;
      end case;
      e := NIL_ELEM;
   end;
   --
end;
