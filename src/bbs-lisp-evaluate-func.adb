with BBS.lisp.conses;
with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.stack;
with BBS.lisp.strings;
with BBS.lisp.symbols;
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
      symb : symbol_ptr;
      flag : Boolean;
      error_occured : Boolean := False;
   begin
      --
      --  Begin should be called at item 2 so that the parameter list is available.
      --
      case p is
         when PH_QUERY =>
            e := (kind => V_INTEGER, i => 4);
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
               p2 := BBS.lisp.conses.get_cdr(s);
               p3 := BBS.lisp.conses.get_car(getList(p2));            --  Should be a symbol or tempsym
               temp := getList(BBS.lisp.conses.get_cdr(getList(p2))); --  Should be parameter list.
               --
               --  Process the function name
               --
               if p3.Kind = V_SYMBOL then
                  symb := p3.sym;
                  if BBS.lisp.symbols.isFixed(symb) then
                     error("defun", "Can't assign a value to a builtin or special symbol.");
                     e := make_error(ERR_UNKNOWN);
                     return;
                  end if;
               elsif p3.Kind = V_TEMPSYM then
                  flag := get_symb(symb, p3.tempsym);
                  if flag then
                     BBS.lisp.strings.ref(p3.tempsym);
                     BBS.lisp.conses.set_car(getList(p2), (kind => V_SYMBOL, sym => symb));
                  else
                     error("defun", "Unable to add symbol.");
                     e := make_error(ERR_UNKNOWN);
                     return;
                  end if;
               else
                  error("defun", "First parameter is not a symbol or temporary symbol.");
                  Put_Line("Parameter type is " & value_type'Image(p3.Kind));
                  e := make_error(ERR_UNKNOWN);
                  return;
               end if;
               --
               --  Next process the parameter list.  Note that currently, defun
               --  is intended to be used at the command level, not within other
               --  functions or local blocks.  Thus there should be no stack
               --  variables to check when processing the parameter list.
               --
               if temp > NIL_CONS then
                  params := BBS.lisp.conses.get_car(temp);
               else
                  error("defun", "Improper parameters.");
                  e := make_error(ERR_UNKNOWN);
                  return;
               end if;
               temp := getList(params);
               BBS.lisp.global.stack.start_frame(error_occured);
               while temp > NIL_CONS loop
                  if isList(BBS.lisp.conses.get_car(temp)) then
                     error("defun", "A parameter cannot be a list.");
                     BBS.lisp.memory.deref(BBS.lisp.conses.get_car(temp));
                     BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                     error_occured := True;
                  end if;
                  declare
                     el : element_type := BBS.lisp.conses.get_car(temp);
                     str : string_index;
                     offset : Natural := 1;
                  begin
                     if el.Kind = V_SYMBOL then
                        if BBS.lisp.symbols.isFixed(el.sym) or (el.sym.kind = ST_FIXED) then
                           error("defun", "Parameter can't be a builtin or special symbol.");
                           BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                           error_occured := True;
                        else
                           msg("defun", "Converting symbol to parameter.");
                           str := BBS.lisp.symbols.get_name(el.sym);
                           el := (Kind => V_STACK, st_name => str,
                               st_offset => offset);
                           BBS.lisp.global.stack.push(str, (kind => V_NONE), error_occured);
                        end if;
                     elsif el.kind = V_TEMPSYM then
                        msg("defun", "Converting tempsym to parameter.");
                        str := el.tempsym;
                        el := (Kind => V_STACK, st_name => str,
                               st_offset => offset);
                        BBS.lisp.global.stack.push(str, (kind => V_NONE), error_occured);
                     else
                        error("defun", "Can't convert item into a parameter.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & value_type'Image(el.kind));
                        BBS.lisp.memory.deref(BBS.lisp.conses.get_car(temp));
                        BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                        error_occured := True;
                     end if;
                     offset := offset + 1;
                     BBS.lisp.conses.set_car(temp, el);
                  end;
                  temp := getList(BBS.lisp.conses.get_cdr(temp));
               end loop;
            else
               error("defun", "Something went horribly wrong and defun did not get a list.");
               error_occured := True;
            end if;
            if error_occured then
               BBS.lisp.memory.deref(params);
               temp := getList(BBS.lisp.conses.get_cdr(getList(p2)));
               BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
               e := make_error(ERR_UNKNOWN);
            end if;
         when PH_PARSE_END =>
            BBS.lisp.global.stack.exit_frame;
            --
            --  EXECUTE Phase
            --
         when PH_EXECUTE =>
            if s = NIL_CONS then
               error("defun", "No parameters given to defun.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            name := BBS.lisp.conses.get_car(s);
            temp := getList(BBS.lisp.conses.get_cdr(s));
            if temp > NIL_CONS then
               params := BBS.lisp.conses.get_car(temp);
            else
               error("defun", "Improper parameters.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            if name.kind /= V_SYMBOL then
               error("defun", "Function name must be a symbol.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            if (not isList(params)) and (params /= NIL_ELEM) then
               error("defun", "Parameter list must be a list or NIL.");
               e := make_error(ERR_UNKNOWN);
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
            if BBS.lisp.symbols.get_type(symb) = SY_VARIABLE then
               BBS.lisp.memory.deref(BBS.lisp.symbols.get_value(symb));
            end if;
            temp := getList(BBS.lisp.conses.get_cdr(s));
            BBS.lisp.conses.set_cdr(s, NIL_ELEM);
            BBS.lisp.symbols.set_sym(symb, (kind => SY_VARIABLE,
                                            pv => (kind => V_LAMBDA,
                                                         lam => temp)));
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
            e := (kind => V_INTEGER, i => 2);
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
               params := BBS.lisp.conses.get_cdr(s);
               if isList(params) then
                  params := BBS.lisp.conses.get_car(getList(params));
               else
                  error("lambda", "Improper parameters.");
                  e := make_error(ERR_UNKNOWN);
                  return;
               end if;
               temp := getList(params);
               BBS.lisp.global.stack.start_frame(error_occured);
               declare
                  el : element_type;
                  str : string_index;
                  offset : Natural := 1;
               begin
                  while temp > NIL_CONS loop
                     if isList(BBS.lisp.conses.get_car(temp)) then
                        error("lambda", "A parameter cannot be a list.");
                        BBS.lisp.memory.deref(BBS.lisp.conses.get_car(temp));
                        BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                        error_occured := True;
                     end if;
                     el :=  BBS.lisp.conses.get_car(temp);
                     if el.kind = V_SYMBOL then
                        if BBS.lisp.symbols.isFixed(el.sym) or (el.sym.kind = ST_FIXED) then
                           error("lambda", "Parameter can't be a builtin or special symbol.");
                           BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                           error_occured := True;
                        else
                           str := BBS.lisp.symbols.get_name(el.sym);
                           msg("lambda", "Converting symbol to parameter");
                           el := (kind => V_STACK, st_name => str,
                               st_offset => offset);
                           BBS.lisp.global.stack.push(str, (kind => V_NONE), error_occured);
                        end if;
                     elsif el.kind = V_TEMPSYM then
                        msg("lambda", "Converting tempsym to parameter");
                        str := el.tempsym;
                        el := (kind => V_STACK, st_name => str, st_offset => offset);
                        BBS.lisp.global.stack.push(str, (kind => V_NONE), error_occured);
                     else
                        error("lambda", "Can't convert item into a parameter.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & value_type'Image(el.kind));
                        BBS.lisp.memory.deref(BBS.lisp.conses.get_car(temp));
                        BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
                        error_occured := True;
                     end if;
                     offset := offset + 1;
                     BBS.lisp.conses.set_car(temp, el);
                     temp := getList(BBS.lisp.conses.get_cdr(temp));
                  end loop;
               end;
            else
               error("lambda", "Something went horribly wrong and lambda did not get a list");
               error_occured := True;
            end if;
            if error_occured then
               BBS.lisp.memory.deref(params);
               temp := getList(BBS.lisp.conses.get_cdr(s));
               BBS.lisp.conses.set_car(temp, make_error(ERR_UNKNOWN));
               e := make_error(ERR_UNKNOWN);
            end if;
         when PH_PARSE_END =>
            BBS.lisp.global.stack.exit_frame;
            --
            --  EXECUTE Phase
            --
         when PH_EXECUTE =>
            if s = NIL_CONS then
               error("lambda", "No parameters given to lambda.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            temp := getList(BBS.lisp.conses.get_car(s));
            if temp > NIL_CONS then
               params := makeList(temp);
            else
               error("defun", "Improper parameters.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            if (not isList(params)) and (params /= NIL_ELEM) then
               error("lambda", "Parameter list must be a list or NIL.");
               e := make_error(ERR_UNKNOWN);
               return;
            end if;
            --
            --  To get to this point, all checks have passed, so return the
            --  parameter list and body.
            --
            BBS.lisp.conses.ref(s);
            e := (kind => V_LAMBDA, lam => s);
            return;
      end case;
      e := NIL_ELEM;
      return;
   end;
   --
   --  Evaluates a lisp form.  Basically this takes the first parameter and passes
   --  it to the evaluator.
   --
   procedure eval_list(e : out element_type; s : cons_index) is
      first_param : element_type;
      t : cons_index := s;
   begin
      if s = NIL_CONS then
         error("eval_list", "No parameter given to eval");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      first_param := first_value(t);
      e := eval(first_param);
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
      temp_value : element_type;
      rest : cons_index;
      name : element_type;
      ret_val : element_type;
      supplied : Integer := 0;
      requested : Integer := 0;
      err : Boolean := False;
   begin
      params := BBS.lisp.conses.get_car(s);
      func_body := BBS.lisp.conses.get_cdr(s);
      supplied := bbs.lisp.utilities.count(e);
      if isList(params) then
         requested := bbs.lisp.utilities.count(getList(params));
      elsif params = NIL_ELEM then
         requested := 0;
      else
         requested := 1;
      end if;
      if supplied /= requested then
         error("function evaluation", "Parameter count mismatch. "  & Integer'Image(supplied)
              & " elements supplied, " & Integer'Image(requested) & " requested.");
         return make_error(ERR_UNKNOWN);
      end if;
      --
      --  Assign parameters to values.
      --
      rest := e;
      name := params;  --  List of parameter names
      BBS.lisp.global.stack.start_frame(err);
      if err then
         error("function evaluation", "Error building stack frame");
         return make_error(ERR_UNKNOWN);
      end if;
      while rest > NIL_CONS loop
         if BBS.lisp.conses.get_car(getList(name)).kind = V_STACK then
            temp_value := first_value(rest);
            BBS.lisp.global.stack.push(BBS.lisp.conses.get_car(getList(name)).st_name,
                                       temp_value, err);
            if err then
               error("function evaluation", "Error adding parameters to stack frame");
               BBS.lisp.global.stack.exit_frame;
               return make_error(ERR_UNKNOWN);
            end if;
            BBS.lisp.memory.deref(temp_value);
         else
            error("function evaluation", "Something horrible happened, a parameter is not a parameter");
            BBS.lisp.global.stack.exit_frame;
            return make_error(ERR_UNKNOWN);
         end if;
         name  := BBS.lisp.conses.get_cdr(getList(name));
      end loop;
      --
      --  Evaluate the function
      --
      ret_val := execute_block(func_body);
      BBS.lisp.global.stack.exit_frame;
      return ret_val;
   end;
end;
