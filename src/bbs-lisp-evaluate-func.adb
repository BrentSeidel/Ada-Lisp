with BBS.lisp.memory;
--with BBS.lisp.strings;
with BBS.lisp.utilities;
with BBS.lisp.stack;
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
   function defun(e : element_type; p : phase) return element_type is
      params : element_type;
      name : element_type;
      temp : element_type;
      p2 : element_type;
      p3 : element_type;
      symb : symb_index;
      flag : Boolean;
   begin
      --
      --  Begin should be called at item 2 so that the parameter list is available.
      --
      case p is
         when PH_QUERY =>
            return (kind => E_VALUE, v => (kind => V_INTEGER, i => 2));
            --
            --  First identify the name, parameter list, and body.  Then perform
            --  initial checks to verify that they are the appropriate kind of object.
            --
         when PH_PARSE_BEGIN =>
            if e.kind = E_CONS then
               --
               --  First process the symbol for the function.
               --
               --            p1 := cons_table(e.ps).car;    --  Should be symbol for defun
               p2 := cons_table(e.ps).cdr;
               p3 := cons_table(p2.ps).car;   --  Should be a symbol or tempsym
               temp := cons_table(p2.ps).cdr; --  Should be parameter list.
               if p3.kind = E_SYMBOL then
                  symb := p3.sym;
                  if (symb_table(symb).kind = SY_BUILTIN) or
                    (symb_table(symb).kind = SY_SPECIAL) then
                     error("defun", "Can't assign a value to a builtin or special symbol");
                     return NIL_ELEM;
                  end if;
               elsif p3.kind = E_TEMPSYM then
                  flag := get_symb(symb, p3.tempsym);
                  if flag then
                     BBS.lisp.memory.ref(p3.tempsym);
                     cons_table(p2.ps).car := (kind => E_SYMBOL, sym => symb);
                  else
                     error("defun", "Unable to add symbol ");
                  end if;
               else
                  error("defun", "First parameter is not a symbol or temporary symbol.");
                  Put_Line("Parameter type is " & ptr_type'Image(p3.kind));
               end if;
               --
               --  Next process the parameter list.
               --
               if temp.kind = E_CONS then
                  params := cons_table(temp.ps).car;
               else
                  error("defun", "Improper parameters.");
               end if;
               temp := params;
               BBS.lisp.stack.start_frame;
               while temp.kind = E_CONS loop
                  if cons_table(temp.ps).car.kind = E_CONS then
                     error("defun", "A parameter cannot be a list.");
                     return NIL_ELEM;
                  end if;
                  declare
                     el : element_type := cons_table(temp.ps).car;
                     str : string_index;
                     offset : stack_index := 1;
                  begin
                     if (el.kind = E_SYMBOL) then
                        str := symb_table(el.sym).str;
                        msg("defun", "Converting symbol to parameter");
                        el := (kind => E_STACK, st_name => str,
                               st_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE, st_name =>
                                               str, st_value => (kind => V_NONE)));
                     elsif (el.kind = E_TEMPSYM) then
                        msg("defun", "Converting tempsym to parameter");
                        str := el.tempsym;
                        BBS.lisp.memory.ref(str);
                        el := (kind => E_STACK, st_name => str,
                               st_offset => offset);
                        BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE, st_name =>
                                               str, st_value => (kind => V_NONE)));
                     else
                        error("defun", "Can't convert item into a parameter.");
                        print(el, False, True);
                        Put_Line("Item is of kind " & ptr_type'Image(el.kind));
                     end if;
                     offset := offset + 1;
                     cons_table(temp.ps).car := el;
                  end;
                  temp := cons_table(temp.ps).cdr;
               end loop;
               BBS.lisp.stack.enter_frame;
            else
               error("defun", "Something went horribly wrong and defun did not get a list");
            end if;
         when PH_PARSE_END =>
            BBS.lisp.stack.exit_frame;
            --
            --  EXECUTE Phase
            --
         when PH_EXECUTE =>
            if e.kind /= E_CONS then
               error("defun", "No parameters given to defun.");
               return NIL_ELEM;
            end if;
            name := cons_table(e.ps).car;
            temp := cons_table(e.ps).cdr;
            if temp.kind = E_CONS then
               params := cons_table(temp.ps).car;
            else
               error("defun", "Improper parameters.");
               return NIL_ELEM;
            end if;
            if (name.kind /= E_SYMBOL)
              and (name.kind /= E_TEMPSYM) then
               error("defun", "Function name must be a symbol or tempsym.");
               return NIL_ELEM;
            end if;
            if (params.kind /= E_CONS) and (params.kind /= E_NIL) then
               error("defun", "Parameter list must be a list or NIL.");
               return NIL_ELEM;
            end if;
            --
            --  If all checks pass, attach the parameter list and body to the
            --  symbol.
            --
            if name.kind = E_TEMPSYM then
               flag := get_symb(symb, name.tempsym);
               if not flag then
                  error("defun", "Unable to add symbol ");
                  BBS.lisp.stack.exit_frame;
                  return NIL_ELEM;
               end if;
            elsif name.kind = E_SYMBOL then
               symb := name.sym;
            end if;
            if (symb_table(symb).kind = SY_BUILTIN) or
              (symb_table(symb).kind = SY_SPECIAL) then
               error("defun", "Cannot redefine builtin symbols");
            else
               temp := cons_table(e.ps).cdr;
               cons_table(e.ps).cdr := NIL_ELEM;
               symb_table(symb) := (ref => 1, str => symb_table(symb).str,
                                 kind => SY_LAMBDA, ps => temp.ps);
               bbs.lisp.memory.ref(temp.ps);
            end if;
      end case;
      return NIL_ELEM;
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
   function eval_function(s : cons_index; e : element_type) return element_type is
      params : element_type ;
      func_body : element_type;
      param_value : value := (kind => V_NONE);
      temp_value : element_type;
      rest : element_type;
      statement : element_type;
      name : element_type;
      ret_val : element_type;
      supplied : Integer := 0;
      requested : Integer := 0;
   begin
      params := cons_table(s).car;
      func_body := cons_table(s).cdr;
      if e.kind = E_CONS then
         supplied := bbs.lisp.utilities.count(e.ps);
      end if;
      if params.kind = E_CONS then
         requested := bbs.lisp.utilities.count(params.ps);
      elsif params.kind = E_NIL then
         requested := 0;
      elsif params.kind /= E_CONS then
         requested := 1;
      end if;
      if supplied /= requested then
         error("function", "Parameter count mismatch. "  & Integer'Image(supplied)
              & " elements supplied, " & Integer'Image(requested) & " requested.");
         return NIL_ELEM;
      end if;
      --
      --  Assign parameters to values.
      --
      rest := e;       --  Supplied parameter values
      name := params;  --  List of parameter names
      BBS.lisp.stack.start_frame;
      while rest.kind = E_CONS loop
         if cons_table(name.ps).car.kind = E_STACK then
            BBS.lisp.utilities.first_value(rest, temp_value, rest);
            if temp_value.kind = E_VALUE then
               param_value := temp_value.v;
            elsif temp_value.kind = E_CONS then
               param_value := (kind => V_LIST, l => temp_value.ps);
            elsif temp_value.kind = E_NIL then
               param_value := (kind => V_BOOLEAN, b => False);
            else
               param_value := (kind => V_NONE);
            end if;
            BBS.lisp.stack.push((kind => BBS.lisp.stack.ST_VALUE,
                                 st_name => cons_table(name.ps).car.st_name,
                                 st_value => param_value));
         else
            error("function evaluation", "Something horrible happened, a parameter is not a parameter");
         end if;
         name  := cons_table(name.ps).cdr;
      end loop;
      BBS.lisp.stack.enter_frame;
      --
      --  Evaluate the function
      --
      statement := func_body;
      ret_val := NIL_ELEM;
      while statement.kind = E_CONS loop
         if cons_table(statement.ps).car.kind = E_CONS then
            ret_val := eval_dispatch(cons_table(statement.ps).car.ps);
         end if;
         statement := cons_table(statement.ps).cdr;
      end loop;
      BBS.lisp.stack.exit_frame;
      return ret_val;
   end;
end;
