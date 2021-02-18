with BBS.lisp.memory;
with BBS.lisp.stack;
package body BBS.lisp.evaluate.vars is
   --
   --  This sets a symbol or stack variable to a value.  The first parameter
   --  must evaluate to a symbol, stack variable, or temp symbol.  If it is a
   --  temp symbol, it is converted to a perminant symbol in the symbol table.
   --  The assigned value is the result of evaluating the second parameter.
   --
   procedure setq(e : out element_type; s : cons_index; p : phase) is
      symb : symb_index;
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
      temp : cons_index;
      str : string_index;
      stacked : Boolean := False;
      index : stack_index;

      procedure deref_previous(s : symb_index) is
      begin
         if symb_table(s).kind = SY_VARIABLE then
            BBS.lisp.memory.deref(symb_table(s).pv);
         end if;
         if symb_table(s).kind = SY_LAMBDA then
            BBS.lisp.memory.deref(symb_table(s).ps);
         end if;
      end;

   begin
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            return;
         when PH_PARSE_BEGIN =>
              msg("setq", "Called during parse begin phase.");
            if s > cons_index'First then
               p1 := cons_table(s).car;  --  Should be symbol for setq
               p2 := cons_table(s).cdr;
               p3 := cons_table(p2.ps).car; --  Should be a symbol or tempsym
               if p3.kind = E_SYMBOL then
                  symb := p3.sym;
                  if (symb_table(symb).kind = SY_BUILTIN) or
                    (symb_table(symb).kind = SY_SPECIAL) then
                     error("setq", "Can't assign a value to a builtin or special symbol");
                     e := (kind => E_ERROR);
                     return;
                  end if;
               elsif p3.kind = E_TEMPSYM then
                  str := p3.tempsym;
                  p3 := find_variable(str, True);
                  BBS.lisp.memory.deref(str);
                  cons_table(p2.ps).car := p3;
               elsif p3.kind = E_STACK then
                  null;
               else
                  error("setq", "First parameter is not a symbol or temporary symbol.");
                  Put_Line("Parameter type is " & ptr_type'Image(p3.kind));
                  e := (kind => E_ERROR);
                  return;
               end if;
            else
               error("setq", "Something went horribly wrong and setq did not get a list");
               e := (kind => E_ERROR);
               return;
            end if;
         when PH_PARSE_END =>
            null;
         when PH_EXECUTE =>
            msg("setq", "Called during execute phase.");
            if s > NIL_CONS then
               p1 := cons_table(s).car;  --  Should be symbol name
               if p1.kind = E_SYMBOL then
                  symb := p1.sym;
               elsif p1.kind = E_STACK then
                  stacked := True;
               else
                  error("setq", "First parameter is not a symbol.");
                  Put_Line("Kind is " & ptr_type'Image(p1.kind));
                  e := (kind => E_ERROR);
                  return;
               end if;
               if not stacked then
                  if (symb_table(symb).kind = SY_BUILTIN) or
                    (symb_table(symb).kind = SY_SPECIAL) then
                     error("setq", "Can't assign a value to a builtin or special symbol");
                     e := (kind => E_ERROR);
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
               if isList(cons_table(s).cdr) then
                  temp := getList(cons_table(s).cdr);
                  p2 := first_value(temp);
               else
                  p2 := cons_table(s).cdr;
               end if;
               BBS.lisp.memory.ref(p2);
                  --
                  --  Check for stack variables
                  --
               if stacked then
                  if p1.kind = E_STACK then
                     index := BBS.lisp.stack.search_frames(p1.st_offset, p1.st_name);
                     BBS.lisp.memory.deref(BBS.lisp.stack.get_entry(index).st_value);
                     if p2.kind = E_VALUE then
                        BBS.lisp.stack.set_value(index, p2.v);
                     elsif p2.kind = E_CONS then
                        BBS.lisp.stack.set_value(index, (kind => V_LIST, l => p2.ps));
                     end if;
                  end if;
               else
                  deref_previous(symb);
                  symb_table(symb) := (ref => 1, Kind => SY_VARIABLE,
                                       pv => p2, str => symb_table(symb).str);
               end if;
               e := p2;
            else
               error("setq", "Not enough arguments.");
               e := (kind => E_ERROR);
            end if;
            return;
      end case;
      e := NIL_ELEM;
   end;
   --
   --  Define local variables and optionally assign values to them.
   --
   procedure local(e : out element_type; s : cons_index; p : phase) is
      locals : element_type;
      list : element_type;
      t : element_type := NIL_ELEM;
   begin
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            return;
         when PH_PARSE_BEGIN =>
            if s > NIL_CONS then
               --
               --  First process the list of local variables
               --
               locals := cons_table(s).cdr;  --  Should be local variable list.
               if locals.kind = E_CONS then
                  locals := cons_table(locals.ps).car;
               else
                  error("let", "Improper parameters.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               BBS.lisp.stack.start_frame;
               while locals.kind = E_CONS loop
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
                     if cons_table(locals.ps).car.kind = E_CONS then
                        el := cons_table(cons_table(locals.ps).car.ps).car;
                     else
                        el := cons_table(locals.ps).car;
                     end if;
                     if el.kind = E_SYMBOL then
                        str := symb_table(el.sym).str;
                        msg("let", "Converting symbol to local variable");
                     elsif el.kind = E_TEMPSYM then
                        str := el.tempsym;
                        msg("let", "Converting tempsym to local variable");
                     elsif el.kind = E_STACK then
                        msg("let", "Converting stack variable to local variable");
                        str := el.st_name;
                     else
                        error("let", "Can't convert item into a local variable.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & ptr_type'Image(el.kind));
--                        BBS.lisp.stack.enter_frame;
                        BBS.lisp.stack.exit_frame;
                        e := (kind => E_ERROR);
                        return;
                     end if;
                     el := (kind => E_STACK, st_name => str,
                            st_offset => offset);
                     BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                          st_name => str,
                                          st_value => (kind => V_NONE)));
                     offset := offset + 1;
                     if cons_table(locals.ps).car.kind = E_CONS then
                        cons_table(cons_table(locals.ps).car.ps).car := el;
                     else
                        cons_table(locals.ps).car := el;
                     end if;
                  end;
                  locals := cons_table(locals.ps).cdr;
               end loop;
            else
               error("let", "Something went horribly wrong and local did not get a list");
               e := (kind => E_ERROR);
               return;
            end if;
         when PH_PARSE_END =>
            BBS.lisp.stack.exit_frame;
         when PH_EXECUTE =>
            --
            --  First process the list of local variables
            --
            locals := cons_table(s).car;  --  Should be parameter list.
            list := cons_table(s).cdr;
            --
            --  Next process the parameter list.
            --
            BBS.lisp.stack.start_frame;
            while locals.kind = E_CONS loop
               --
               --  If the local variable is a cons, then the first element
               --  is the variable name and the second element is the value.
               --
               declare
                  el : element_type;
                  check : element_type;
                  offset : Natural := 1;
                  local_val : value := (kind => V_BOOLEAN, b => False);
               begin
                  if cons_table(locals.ps).car.kind = E_CONS then
                     el := cons_table(cons_table(locals.ps).car.ps).car;
                     --
                     -- Check if there is a value
                     --
                     check := cons_table(cons_table(locals.ps).car.ps).cdr;
                     if check.kind = E_CONS then
                        check := cons_table(check.ps).car;
                     end if;
                     if check.kind = E_CONS then
                        check := eval_dispatch(check.ps);
                     else
                        check := indirect_elem(check);
                     end if;
                     case check.kind is
                        when E_CONS =>
                           local_val := (kind => V_LIST, l => check.ps);
                           null;
                        when E_VALUE =>
                           local_val := check.v;
                        when others =>  -- Might need to update to check for errors.
                           null;
                     end case;
                  else
                     el := cons_table(locals.ps).car;
                  end if;
                  if (el.kind = E_STACK) then
                     BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                          st_name => el.st_name,
                                          st_value => local_val));
                  else
                     error("let", "Local variable is not a local.");
                     print(el, False, True);
                     Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                     BBS.lisp.stack.exit_frame;
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  offset := offset + 1;
               end;
               locals := cons_table(locals.ps).cdr;
            end loop;
            --
            --  Now evaluate the statements in this context.
            --
            t := execute_block(list);
            if t.kind = E_ERROR then
               error("let", "Error occured evaluting statement");
            end if;
            BBS.lisp.stack.exit_frame;
            e := t;
            return;
      end case;
      e := NIL_ELEM;
   end;
   --
end;
