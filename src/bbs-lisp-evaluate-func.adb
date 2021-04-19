with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.func is
   --
   --  Defines a function.  The command is (defun name (parameters) body).
   --    name is a symbol of type LAMBDA.
   --    params is a list of the parameters for the function.  It must be a
   --      list of elements that translate to symbols or tempsyms.  Defun translates
   --      these to parameter elements.
   --    body is a list of the actions for the function.  This needs to be
   --      scanned and any symbol or tempsym that matches one of the params is
   --      translated to point to the parameter atom in the parameter list.  It
   --      also could concievable be a single atom or even NIL.
   --
   procedure defun(e : out element_type; s : cons_index; p : phase) is
      params : element_type;
      name : element_type;
      temp : cons_index;
      p2 : element_type;
      p3 : element_type;
      symb : symb_index;
      flag : Boolean;
      error_occured : Boolean := False;
   begin
      --
      --  Begin should be called at item 2 so that the parameter list is available.
      --
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 2));
            return;
            --
            --  First identify the name, parameter list, and body.  Then perform
            --  initial checks to verify that they are the appropriate kind of object.
            --
         when PH_PARSE_BEGIN =>
            if s > NIL_CONS then
               --
               --  First process the symbol for the function.
               --
               p2 := cons_table(s).cdr;
               p3 := cons_table(p2.ps).car;   --  Should be a symbol or tempsym
               temp := getList(cons_table(p2.ps).cdr); --  Should be parameter list.
               --
               --  Process the function name
               --
               if p3.Kind = E_SYMBOL then
                  symb := p3.sym;
                  if (symb_table(symb).Kind = SY_BUILTIN) or
                    (symb_table(symb).Kind = SY_SPECIAL) then
                     error("defun", "Can't assign a value to a builtin or special symbol.");
                     e := (Kind => E_ERROR);
                     return;
                  end if;
               elsif p3.Kind = E_TEMPSYM then
                  flag := get_symb(symb, p3.tempsym);
                  if flag then
                     BBS.lisp.memory.ref(p3.tempsym);
                     cons_table(p2.ps).car := (Kind => E_SYMBOL, sym => symb);
                  else
                     error("defun", "Unable to add symbol.");
                     e := (Kind => E_ERROR);
                     return;
                  end if;
               else
                  error("defun", "First parameter is not a symbol or temporary symbol.");
                  Put_Line("Parameter type is " & ptr_type'Image(p3.Kind));
                  e := (Kind => E_ERROR);
                  return;
               end if;
               --
               --  Next process the parameter list.  Note that currently, defun
               --  is intended to be used at the command level, not within other
               --  functions or local blocks.  Thus there should be no stack
               --  variables to check when processing the parameter list.
               --
               if temp > NIL_CONS then
                  params := cons_table(temp).car;
               else
                  error("defun", "Improper parameters.");
                  e := (Kind => E_ERROR);
                  return;
               end if;
               temp := getList(params);
               BBS.lisp.stack.start_frame(error_occured);
               while temp > NIL_CONS loop
                  if isList(cons_table(temp).car) then
                     error("defun", "A parameter cannot be a list.");
                     BBS.lisp.memory.deref(cons_table(temp).car);
                     cons_table(temp).car := (Kind => E_ERROR);
                     error_occured := True;
                  end if;
                  declare
                     el : element_type := cons_table(temp).car;
                     str : string_index;
                     offset : Natural := 1;
                  begin
                     if (el.Kind = E_SYMBOL) then
                        if (symb_table(el.sym).Kind = SY_BUILTIN) or
                          (symb_table(el.sym).Kind = SY_SPECIAL) then
                           error("defun", "Parameter can't be a builtin or special symbol.");
                           cons_table(temp).car := (Kind => E_ERROR);
                           error_occured := True;
                        else
                           msg("defun", "Converting symbol to parameter.");
                           str := symb_table(el.sym).str;
                           el := (Kind => E_STACK, st_name => str,
                               st_offset => offset);
                           BBS.lisp.stack.push((Kind => BBS.lisp.stack.ST_VALUE,
                                                st_name => str, st_value => (kind => V_NONE)),
                                               error_occured);
                        end if;
                     elsif (el.kind = E_TEMPSYM) then
                        msg("defun", "Converting tempsym to parameter.");
                        str := el.tempsym;
                        el := (Kind => E_STACK, st_name => str,
                               st_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                             st_name => str, st_value => (kind => V_NONE)),
                                           error_occured);
                     else
                        error("defun", "Can't convert item into a parameter.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                        BBS.lisp.memory.deref(cons_table(temp).car);
                        cons_table(temp).car := (Kind => E_ERROR);
                        error_occured := True;
                     end if;
                     offset := offset + 1;
                     cons_table(temp).car := el;
                  end;
                  temp := getList(cons_table(temp).cdr);
               end loop;
            else
               error("defun", "Something went horribly wrong and defun did not get a list.");
               error_occured := True;
            end if;
            if error_occured then
               BBS.lisp.memory.deref(params);
               temp := getList(cons_table(p2.ps).cdr);
               cons_table(temp).car := (Kind => E_ERROR);
               e := (Kind => E_ERROR);
            end if;
         when PH_PARSE_END =>
            BBS.lisp.stack.exit_frame;
            --
            --  EXECUTE Phase
            --
         when PH_EXECUTE =>
            if s = NIL_CONS then
               error("defun", "No parameters given to defun.");
               e := (kind => E_ERROR);
               return;
            end if;
            name := cons_table(s).car;
            temp := getList(cons_table(s).cdr);
            if temp > NIL_CONS then
               params := cons_table(temp).car;
            else
               error("defun", "Improper parameters.");
               e := (kind => E_ERROR);
               return;
            end if;
            if (name.kind /= E_SYMBOL) then
               error("defun", "Function name must be a symbol.");
               e := (kind => E_ERROR);
               return;
            end if;
            if (not isList(params)) and (params.kind /= E_NIL) then
               error("defun", "Parameter list must be a list or NIL.");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  To get to this point, all checks have passed, so attach the
            --  parameter list and body to the symbol.
            --
            symb := name.sym;
            --
            --  If something else was attached to the symbol, deref it.
            --
            if symb_table(symb).kind = SY_LAMBDA then
               BBS.lisp.memory.deref(symb_table(symb).ps);
            elsif symb_table(symb).kind = SY_VARIABLE then
               BBS.lisp.memory.deref(symb_table(symb).pv);
            end if;
            temp := getList(cons_table(s).cdr);
            cons_table(s).cdr := NIL_ELEM;
            symb_table(symb) := (ref => 1, str => symb_table(symb).str,
                                 kind => SY_LAMBDA, ps => temp);
      end case;
      e := NIL_ELEM;
   end;
   --
   --  Defines a function.  The command is (lambda (parameters) body).
   --    params is a list of the parameters for the function.  It must be a
   --      list of elements that translate to symbols or tempsyms.  Defun translates
   --      these to parameter elements.
   --    body is a list of the actions for the function.  This needs to be
   --      scanned and any symbol or tempsym that matches one of the params is
   --      translated to point to the parameter atom in the parameter list.  It
   --      also could concievable be a single atom or even NIL.
   --    The returned value is an variable element of type V_LAMBDA.
   --
   procedure lambda(e : out element_type; s : cons_index; p : phase) is
      params : element_type;
      temp : cons_index;
      error_occured : Boolean := False;
   begin
      --
      --  Begin should be called at item 2 so that the parameter list is available.
      --
      case p is
         when PH_QUERY =>
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            return;
            --
            --  First identify the name, parameter list, and body.  Then perform
            --  initial checks to verify that they are the appropriate kind of object.
            --
         when PH_PARSE_BEGIN =>
            if s > NIL_CONS then
               --
               --  Process the parameter list.  Note that currently, defun
               --  is intended to be used at the command level, not within other
               --  functions or local blocks.  Thus there should be no stack
               --  variables to check when processing the parameter list.
               --
               params := cons_table(s).cdr;
               if isList(params) then
                  params := cons_table(getList(params)).car;
               else
                  error("lambda", "Improper parameters.");
                  e := (kind => E_ERROR);
                  return;
               end if;
               temp := getList(params);
               BBS.lisp.stack.start_frame(error_occured);
               declare
                  el : element_type;
                  str : string_index;
                  offset : Natural := 1;
               begin
                  while temp > NIL_CONS loop
                     if isList(cons_table(temp).car) then
                        error("lambda", "A parameter cannot be a list.");
                        BBS.lisp.memory.deref(cons_table(temp).car);
                        cons_table(temp).car := (Kind => E_ERROR);
                        error_occured := True;
                     end if;
                     el :=  cons_table(temp).car;
                     if (el.kind = E_SYMBOL) then
                        if (symb_table(el.sym).Kind = SY_BUILTIN) or
                          (symb_table(el.sym).Kind = SY_SPECIAL) then
                           error("lambda", "Parameter can't be a builtin or special symbol.");
                           cons_table(temp).car := (Kind => E_ERROR);
                           error_occured := True;
                        else
                           str := symb_table(el.sym).str;
                           msg("lambda", "Converting symbol to parameter");
                           el := (kind => E_STACK, st_name => str,
                               st_offset => offset);
                           BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                                st_name => str, st_value => (kind => V_NONE)),
                                              error_occured);
                        end if;
                     elsif (el.kind = E_TEMPSYM) then
                        msg("lambda", "Converting tempsym to parameter");
                        str := el.tempsym;
                        el := (kind => E_STACK, st_name => str,
                               st_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                             st_name => str, st_value => (kind => V_NONE)),
                                           error_occured);
                     else
                        error("lambda", "Can't convert item into a parameter.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                        BBS.lisp.memory.deref(cons_table(temp).car);
                        cons_table(temp).car := (Kind => E_ERROR);
                        error_occured := True;
                     end if;
                     offset := offset + 1;
                     cons_table(temp).car := el;
                     temp := getList(cons_table(temp).cdr);
                  end loop;
               end;
            else
               error("lambda", "Something went horribly wrong and lambda did not get a list");
               error_occured := True;
            end if;
            if error_occured then
               BBS.lisp.memory.deref(params);
               temp := getList(cons_table(s).cdr);
               cons_table(temp).car := (Kind => E_ERROR);
               e := (Kind => E_ERROR);
            end if;
         when PH_PARSE_END =>
            BBS.lisp.stack.exit_frame;
            --
            --  EXECUTE Phase
            --
         when PH_EXECUTE =>
            if s = NIL_CONS then
               error("lambda", "No parameters given to lambda.");
               e := (kind => E_ERROR);
               return;
            end if;
            temp := getList(cons_table(s).car);
            if temp > NIL_CONS then
               params := makeList(temp);
            else
               error("defun", "Improper parameters.");
               e := (kind => E_ERROR);
               return;
            end if;
            if (not isList(params)) and (params.kind /= E_NIL) then
               error("lambda", "Parameter list must be a list or NIL.");
               e := (kind => E_ERROR);
               return;
            end if;
            --
            --  To get to this point, all checks have passed, so return the
            --  parameter list and body.
            --
            BBS.lisp.memory.ref(s);
            e := (kind => E_VALUE, v => (kind => V_LAMBDA, lam => s));
            return;
      end case;
      e := NIL_ELEM;
      return;
   end;
   --
   --  Functions for evaluating lisp functions.
   --    s points to the function definition
   --    e points to the parameters being passed to the function
   --
   --  A function definition is stored as two lists.  The first is a list of
   --  the parameters that can be passed to the function.  The second is the
   --  function body.
   --
   function eval_function(s : cons_index; e : cons_index) return element_type is
      params : element_type ;
      func_body : element_type;
      param_value : value := (kind => V_NONE);
      temp_value : element_type;
      rest : cons_index;
      name : element_type;
      ret_val : element_type;
      supplied : Integer := 0;
      requested : Integer := 0;
      err : Boolean := False;
   begin
      params := cons_table(s).car;
      func_body := cons_table(s).cdr;
      supplied := bbs.lisp.utilities.count(e);
      if isList(params) then
         requested := bbs.lisp.utilities.count(getList(params));
      elsif params.kind = E_NIL then
         requested := 0;
      else
         requested := 1;
      end if;
      if supplied /= requested then
         error("function evaluation", "Parameter count mismatch. "  & Integer'Image(supplied)
              & " elements supplied, " & Integer'Image(requested) & " requested.");
         return (kind => E_ERROR);
      end if;
      --
      --  Assign parameters to values.
      --
      rest := e;
      name := params;  --  List of parameter names
      BBS.lisp.stack.start_frame(err);
      if err then
         error("function evaluation", "Error building stack frame");
         return (Kind => E_ERROR);
      end if;
      while rest > NIL_CONS loop
         if cons_table(name.ps).car.kind = E_STACK then
--            put("eval_function: Parameter name is ");
--            print(cons_table(name.ps).car.st_name);
--            new_line;
            temp_value := first_value(rest);
--            put_line("eval_function: Processed parameter kind is " & ptr_type'Image(temp_value.kind));
            if temp_value.kind = E_VALUE then
               param_value := temp_value.v;
            elsif temp_value.kind = E_SYMBOL then
--               put_line("eval_function: Parameter is symbol of type " & symbol_type'Image(symb_table(temp_value.sym).kind));
               param_value := (kind => V_SYMBOL, sym => temp_value.sym);
            elsif isList(temp_value) then
               param_value := (kind => V_LIST, l => getList(temp_value));
            elsif temp_value.kind = E_NIL then
               param_value := (kind => V_BOOLEAN, b => False);
            else
               param_value := (kind => V_NONE);
            end if;
            BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                 st_name => cons_table(name.ps).car.st_name,
                                 st_value => param_value), err);
            if err then
               error("function evaluation", "Error adding parameters to stack frame");
               BBS.lisp.stack.exit_frame;
               return (Kind => E_ERROR);
            end if;
            BBS.lisp.memory.deref(param_value);
         else
            error("function evaluation", "Something horrible happened, a parameter is not a parameter");
            BBS.lisp.stack.exit_frame;
            return (kind => E_ERROR);
         end if;
         name  := cons_table(name.ps).cdr;
      end loop;
      --
      --  Evaluate the function
      --
      ret_val := execute_block(func_body);
      BBS.lisp.stack.exit_frame;
      return ret_val;
   end;
end;
