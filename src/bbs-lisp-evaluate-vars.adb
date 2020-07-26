with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.vars is
   --
   --  This sets a symbol to a value.  The first parameter must evaluate to a
   --  symbol or temp symbol.  If it is a temp symbol, it is converted to a
   --  perminant symbol in the symbol table.  The assigned value is the result
   --  of evaluating the second parameter.
   --
   --  To improve memory management, need to make a copy of the assigned value
   --  to use as a return value.  Currently, the REPL deallocates the returned
   --  value after printing it.
   --
   function setq(e : element_type; p : phase) return element_type is
      symb : symb_index;
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
      ret : element_type;
      stacked : Boolean := False;
      index : stack_index;

      procedure deref_previous(s : symb_index) is
      begin
         if symb_table(s).kind = VARIABLE then
            BBS.lisp.memory.deref(symb_table(s).pv);
         end if;
         if symb_table(s).kind = LAMBDA then
            BBS.lisp.memory.deref(symb_table(s).ps);
         end if;
      end;

   begin
      if p = QUERY then
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
      elsif p = PARSE_BEGIN then
         msg("setq", "Called during parse begin phase.");
         if e.kind = E_CONS then
            p1 := cons_table(e.ps).car;  --  Should be symbol for setq
            p2 := cons_table(e.ps).cdr;
            p3 := cons_table(p2.ps).car; --  Should be a symbol or tempsym
            if p3.kind = E_SYMBOL then
               symb := p3.sym;
               if (symb_table(symb).kind = BUILTIN) or
                 (symb_table(symb).kind = SPECIAL) then
                  error("setq", "Can't assign a value to a builtin or special symbol");
                  return NIL_ELEM;
               end if;
            elsif p3.kind = E_TEMPSYM then
               p3 := find_variable(p3.tempsym, True);
               Put_Line("Setq: Found variable of type " & ptr_type'Image(p3.kind));
               cons_table(p2.ps).car := p3;
            elsif (p3.kind = E_LOCAL) or (p3.kind = E_PARAM) then
               null;
            else
               error("setq", "First parameter is not a symbol or temporary symbol.");
               Put_Line("Parameter type is " & ptr_type'Image(p3.kind));
            end if;
         else
            error("setq", "Something went horribly wrong and setq did not get a list");
         end if;
      elsif p = PARSE_END then
         null;
      elsif p = EXECUTE then
         msg("setq", "Called during execute phase.");
         if e.kind = E_CONS then
            p1 := cons_table(e.ps).car;  --  Should be symbol name
            p2 := cons_table(e.ps).cdr;  --  Should be value to be assigned
            if p1.kind = E_SYMBOL then
               symb := p1.sym;
            elsif (p1.kind = E_PARAM) or (p1.kind = E_LOCAL) then
               stacked := True;
            else
               error("setq", "First parameter is not a symbol.");
               Put_Line("Kind is " & ptr_type'Image(p1.kind));
               return NIL_ELEM;
            end if;
            if not stacked then
               if (symb_table(symb).kind = BUILTIN) or
                 (symb_table(symb).kind = SPECIAL) then
                  error("setq", "Can't assign a value to a builtin or special symbol");
                  return NIL_ELEM;
               end if;
            end if;
            --
            --  At this point, p1 should be an element containing a valid symbol and
            --  symb is the index to that symbol.
            --
            --
            --  Now determine what value to attach to the symbol.
            --
            if p2.kind = E_CONS then
               p3 := cons_table(p2.ps).car;
               if p3.kind = E_CONS then
                  ret := eval_dispatch(p3.ps);
               else -- p3 is an element
                  ret := BBS.lisp.utilities.indirect_elem(p3);
               end if;
               --
               --  Check for stack variables
               --
               if stacked then
                  if p1.kind = E_PARAM then
                     index := BBS.lisp.stack.search_frames(p1.p_offset, p1.p_name);
                     BBS.lisp.memory.deref((kind => E_VALUE, v => BBS.lisp.stack.stack(index).p_value));
                     BBS.lisp.memory.ref(ret);
                     if ret.kind = E_VALUE then
                        BBS.lisp.stack.stack(index).p_value := ret.v;
                     elsif ret.kind = E_CONS then
                        BBS.lisp.stack.stack(index).p_value := (kind => V_LIST, l => ret.ps);
                     end if;
                  elsif p1.kind = E_LOCAL then
                     index := BBS.lisp.stack.search_frames(p1.l_offset, p1.l_name);
                     BBS.lisp.memory.deref((kind => E_VALUE, v => BBS.lisp.stack.stack(index).l_value));
                     BBS.lisp.memory.ref(ret);
                     if ret.kind = E_VALUE then
                        BBS.lisp.stack.stack(index).l_value := ret.v;
                     elsif ret.kind = E_CONS then
                        BBS.lisp.stack.stack(index).l_value := (kind => V_LIST, l => ret.ps);
                     end if;
                  end if;
               else
                  deref_previous(symb);
                  BBS.lisp.memory.ref(ret);
                  symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                       pv => ret, str => symb_table(symb).str);
               end if;
               return ret;
            elsif p2.kind = E_VALUE then -- Rare, CDR is an value.
               deref_previous(symb);
               --
               --  At some point, this needs to be updated to check for stack variables.
               --
               symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                 pv => p2, str => symb_table(symb).str);
               return p2;
            else
               error("setq", "Not enough arguments.");
            end if;
         else
            error("setq", "Not enough arguments.");
         end if;
      end if;
      return NIL_ELEM;
   end;
   --
   --  Define local variables and optionally assign values to them.
   --
   function local(e : element_type; p : phase) return element_type is
--      p1 : element_type;
      locals : element_type;
      list : element_type;
      ptr : element_type;
      t : element_type := NIL_ELEM;
   begin
      case p is
         when QUERY =>
            return (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
         when PARSE_BEGIN =>
            if e.kind = E_CONS then
               --
               --  First process the list of local variables
               --
--               locals := cons_table(e.ps).car;  --  Should be symbol for local
               locals := cons_table(e.ps).cdr;  --  Should be local variable list.
               --
               --  Next process the local variable list.
               --
               if locals.kind = E_CONS then
                  locals := cons_table(locals.ps).car;
               else
                  error("local", "Improper parameters.");
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
                     offset : stack_index := 1;
                  begin
                     if cons_table(locals.ps).car.kind = E_CONS then
                        el := cons_table(cons_table(locals.ps).car.ps).car;
                     else
                        el := cons_table(locals.ps).car;
                     end if;
                     if el.kind = E_SYMBOL then
                        str := symb_table(el.sym).str;
                        msg("local", "Converting symbol to local variable");
                        el := (kind => E_LOCAL, l_name => str,
                               l_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_LOCAL,
                                             l_name => str,
                                             l_value => (kind => V_NONE)));
                     elsif el.kind = E_TEMPSYM then
                        msg("local", "Converting tempsym to local variable");
                        str := el.tempsym;
                        el := (kind => E_LOCAL, l_name => str,
                               l_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_LOCAL,
                                             l_name => str,
                                             l_value => (kind => V_NONE)));
                     elsif el.kind = E_PARAM then
                        str := el.p_name;
                        el := (kind => E_LOCAL, l_name => str,
                               l_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_LOCAL,
                                             l_name => str,
                                             l_value => (kind => V_NONE)));
                     elsif el.kind = E_LOCAL then
                        str := el.l_name;
                        el := (kind => E_LOCAL, l_name => str,
                               l_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_LOCAL,
                                             l_name => str,
                                             l_value => (kind => V_NONE)));
                     else
                        error("local", "Can't convert item into a local variable.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                     end if;
                     offset := offset + 1;
                     if cons_table(locals.ps).car.kind = E_CONS then
                        cons_table(cons_table(locals.ps).car.ps).car := el;
                     else
                        cons_table(locals.ps).car := el;
                     end if;
                  end;
                  locals := cons_table(locals.ps).cdr;
               end loop;
               BBS.lisp.stack.enter_frame;
            else
               error("local", "Something went horribly wrong and local did not get a list");
            end if;
         when PARSE_END =>
            BBS.lisp.stack.exit_frame;
         when EXECUTE =>
            --
            --  First process the list of local variables
            --
            locals := cons_table(e.ps).car;  --  Should be parameter list.
            list := cons_table(e.ps).cdr;
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
                  offset : stack_index := 1;
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
                        check := bbs.lisp.utilities.indirect_elem(check);
                     end if;
                     case check.kind is
                        when E_CONS =>
                           local_val := (kind => V_LIST, l => check.ps);
                           null;
                        when E_VALUE =>
                           local_val := check.v;
                        when others =>
                           null;
                     end case;
                  else
                     el := cons_table(locals.ps).car;
                  end if;
                  if (el.kind = E_LOCAL) then
                     BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_LOCAL,
                                          l_name => el.l_name,
                                          l_value => local_val));
                  else
                     error("local", "Local variable is not a local.");
                     print(el, False, True);
                     Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                  end if;
                  offset := offset + 1;
               end;
               locals := cons_table(locals.ps).cdr;
            end loop;
            BBS.lisp.stack.enter_frame;
            --
            --  Now evaluate the statements in this context.
            --
            ptr := list;
            while ptr.kind /= E_NIL loop
               if ptr.kind = E_CONS then
                  if cons_table(ptr.ps).car.kind = E_CONS then
                     t := eval_dispatch(cons_table(ptr.ps).car.ps);
                  else
                     t := bbs.lisp.utilities.indirect_elem(cons_table(ptr.ps).car);
                  end if;
                  ptr := cons_table(ptr.ps).cdr;
               else
                  ptr := NIL_ELEM;
               end if;
            end loop;
            BBS.lisp.stack.exit_frame;
            return t;
      end case;
      return NIL_ELEM;
   end;
   --
end;
