with bbs.lisp.memory;
with bbs.lisp.strings;
with BBS.lisp.utilities;

package body bbs.lisp.evaluate is
   --
   function newline(e : element_type) return element_type is
   begin
      New_Line;
      return NIL_ELEM;
   end;
   --
   function reset(e : element_type) return element_type is
   begin
      init;
      return NIL_ELEM;
   end;
   --
   function msg_on(e : element_type) return element_type is
   begin
      msg_flag := True;
      return NIL_ELEM;
   end;
   --
   function msg_off(e : element_type) return element_type is
   begin
      msg_flag := False;
      return NIL_ELEM;
   end;
   --
   function quote(e : element_type) return element_type is
      temp : element_type := e;
   begin
      bbs.lisp.memory.ref(temp);
      return temp;
   end;
   --
   function dump(e : element_type) return element_type is
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      return NIL_ELEM;
   end;
   --
   --  This function evaluates the basic arithmatic operation (+, -, *, /).
   --
   function eval_math(e : element_type; b : mathops) return element_type is
      accum : Integer := 0;
      v : value;
      p : cons_index;
      el: element_type;
      temp : element_type;
      --
      --  Subfunction to do the actual evaluation.
      --
      function process_value(e : element_type; accum : Integer; b : mathops) return Integer is
         v  : value;
         e1 : element_type;
      begin
         e1 := bbs.lisp.utilities.indirect_elem(e);
         if e1.kind = E_VALUE then
            v := e.v;
            if v.kind = V_INTEGER then
               case (b) is
               when PLUS =>
                  return accum + v.i;
               when MUL =>
                  return accum * v.i;
               when MINUS =>
                  return accum - v.i;
               when DIV =>
                  return accum / v.i;
               when others =>
                  error("eval_math.process_atom", "Internal error, unknown math operation");
               end case;
            else
               error("eval_math.process_atom", "Can't process " & value_type'Image(v.kind));
            end if;
         else
            error("eval_math.process_atom", "Can't process " & ptr_type'Image(e1.kind));
         end if;
         return accum;
      end;

   begin
      if e.kind = E_VALUE then
         if e.v.kind = V_INTEGER then
            accum := e.v.i;
         end if;
      elsif e.kind = E_CONS then
         p := e.ps;
         if cons_table(p).car.kind /= E_CONS then
            el := BBS.lisp.utilities.indirect_elem(cons_table(p).car);
            if el.kind = E_VALUE then
               v := el.v;
            else
              error("eval_math", "Can't process element " & ptr_type'Image(el.kind));
            end if;
            if v.kind = V_INTEGER then
               accum := v.i;
            else
               error("eval_math", "Can't process " & value_type'Image(v.kind));
            end if;
         elsif cons_table(p).car.kind = E_CONS then
            temp := eval_dispatch(cons_table(p).car.ps);
            if temp.kind /= E_CONS then
               el := bbs.lisp.utilities.indirect_elem(temp);
               if el.kind = E_VALUE then
                  v := el.v;
               end if;
               if v.kind = V_INTEGER then
                  accum := v.i;
               else
                  error("eval_math", "Can't process " & value_type'Image(v.kind));
               end if;
            end if;
            bbs.lisp.memory.deref(temp);
         end if;
         if cons_table(p).cdr.kind /= E_NIL then
            p := cons_table(p).cdr.ps;
            loop
               if cons_table(p).car.kind = E_CONS then
                  temp := eval_dispatch(cons_table(p).car.ps);
                  if temp.kind = E_VALUE then
                     accum := process_value(temp, accum, b);
                  end if;
                  bbs.lisp.memory.deref(temp);
               else
                  el := bbs.lisp.utilities.indirect_elem(cons_table(p).car);
                  accum := process_value(el, accum, b);
               end if;
               exit when cons_table(p).cdr.kind /= E_CONS;
               p := cons_table(p).cdr.ps;
            end loop;
            if cons_table(p).cdr.kind /= E_NIL then
               el := bbs.lisp.utilities.indirect_elem(cons_table(p).cdr);
               accum := process_value(el, accum, b);
            end if;
         end if;
      end if;
      return (Kind => E_VALUE, v => (kind => V_INTEGER, i => accum));
   end;
   --
   function add(e : element_type) return element_type is
   begin
      return eval_math(e, PLUS);
   end;
   --
   function sub(e : element_type) return element_type is
   begin
      return eval_math(e, MINUS);
   end;
   --
   function mul(e : element_type) return element_type is
   begin
      return eval_math(e, MUL);
   end;
   --
   function div(e : element_type) return element_type is
   begin
      return eval_math(e, DIV);
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
   function car(e : element_type) return element_type is
      first : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         BBS.lisp.memory.ref(cons_table(s).car);
         BBS.lisp.memory.deref(first);
         return cons_table(s).car;
      end if;
      return first;
   end;
   --
   --  Return the rest of a list
   --
   function cdr(e : element_type) return element_type is
      first : element_type;
      rest : element_type;
      s : cons_index;
   begin
      BBS.lisp.utilities.first_value(e, first, rest);
      if BBS.lisp.utilities.isList(first) then
         s := BBS.lisp.utilities.getList(first);
         BBS.lisp.memory.ref(cons_table(s).cdr);
         BBS.lisp.memory.deref(first);
         return cons_table(s).cdr;
      end if;
      return NIL_ELEM;
   end;
   --
   --  Perform comparison operations.
   --
   function eval_comp(e : element_type; b : compops) return element_type is
      t  : element_type;
      t1 : element_type;
      t2 : element_type;
      v1 : value;
      v2 : value;
      el : element_type;
      i1 : Integer;
      i2 : Integer;
   begin
      if e.kind = E_CONS then
         BBS.lisp.utilities.first_value(e, t1, t);
         if t.kind = E_CONS then
            BBS.lisp.utilities.first_value(t, t2, t);
         else
            error("eval_comp", "Cannot compare a single atom.");
            return NIL_ELEM;
         end if;
      else
         error("eval_comp", "Cannot compare a single atom.");
         return NIL_ELEM;
      end if;
      if (t1.kind /= E_CONS) and (t2.kind /= E_CONS) then
         t := bbs.lisp.utilities.indirect_elem(t1);
         if t.kind = E_VALUE then
            v1 := t.v;
         end if;
         t := bbs.lisp.utilities.indirect_elem(t2);
         if t.kind = E_VALUE then
            v2 := t.v;
         end if;
         if v1.kind = V_INTEGER and
           v2.kind = V_INTEGER then
            i1 := v1.i;
            i2 := v2.i;
            case b is
               when SYM_EQ =>
                  if i1 = i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_NE =>
                  if i1 /= i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_LT =>
                  if i1 < i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when SYM_GT =>
                  if i1 > i2 then
                     return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                  end if;
               when others =>
                  error("eval_comp", "Unknown comparison type.");
               end case;
               return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         elsif v1.kind = V_STRING and
           v2.kind = V_STRING then
            declare
               eq : comparison;
            begin
               eq := bbs.lisp.strings.compare(v1.s, v2.s);
               case b is
                  when SYM_EQ =>
                     if eq = CMP_EQ then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_LT =>
                     if eq = CMP_LT then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_GT =>
                     if eq = CMP_GT then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when SYM_NE =>
                     if eq /= CMP_EQ then
                        return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
                     end if;
                  when others =>
                     error("eval_comp", "Unknown comparison type.");
               end case;
            end;
            return (Kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         else
            error("eval_comp", "Can only compare integers, strings, or symbols.");
            return NIL_ELEM;
         end if;
      else
         error("eval_comp", "Can only compare two atoms.");
         return NIL_ELEM;
      end if;
   end;
   --
   function eq(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_EQ);
   end;
   --
   function ne(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_NE);
   end;
   --
   function lt(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_LT);
   end;
   --
   function gt(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_GT);
   end;
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
      tempsym : tempsym_index;
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
      ret : element_type;
      flag : Boolean;

      procedure deref_previous(s : symb_index) is
      begin
         if symb_table(symb).kind = VARIABLE then
            bbs.lisp.memory.deref(symb_table(symb).pv);
         end if;
         if symb_table(symb).kind = LAMBDA then
            bbs.lisp.memory.deref(symb_table(symb).ps);
         end if;
      end;

   begin
      if p = PARSE then
         null;
      elsif p = EXECUTE then
         if e.kind = E_CONS then
            p1 := cons_table(e.ps).car;  --  Should be symbol name
            p2 := cons_table(e.ps).cdr;  --  Should be value to be assigned
            if p1.kind = E_SYMBOL then
               symb := p1.sym;
            elsif p1.kind = E_TEMPSYM then
               tempsym := p1.tempsym;
               flag := get_symb(symb, string_index(tempsym_table(tempsym)));
               if not flag then
                  error("setq", "Unable to add symbol ");
                  return NIL_ELEM;
               end if;
            else
               error("setq", "First parameter is not a symbol or temporary symbol.");
               return NIL_ELEM;
            end if;
            if symb_table(symb).kind = BUILTIN then
               error("setq", "Can't set value of builtin symbol ");
               return NIL_ELEM;
            end if;
            --
            --  At this point, p1 should be an atom containing a valid symbol and
            --  symb is the index to that symbol.
            --
            --
            --  Now determine what value to attach to the symbol.
            --
            if p2.kind = E_CONS then
               p3 := cons_table(p2.ps).car;
               if p3.kind = E_CONS then
                  ret := eval_dispatch(p3.ps);
                  deref_previous(symb);
                  BBS.lisp.memory.ref(ret);
                  symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                    pv => ret, str => symb_table(symb).str);
                  return ret;
               else -- p3 points to an atom
                  deref_previous(symb);
                  symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                    pv => p3, str => symb_table(symb).str);
                  return p3;
               end if;
            elsif p2.kind = E_VALUE then -- Rare, CDR is an value.
               deref_previous(symb);
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
   --  Print stuff
   --
   function print(e : element_type) return element_type is
      t : element_type := e;
      result : element_type;
      car : element_type;
   begin
      while t.kind = E_CONS loop
         car := cons_table(t.ps).car;
         if car.kind /= E_CONS then
            result := bbs.lisp.utilities.indirect_elem(car);
            print(result, False, False);
         elsif car.kind = E_CONS then
            result := eval_dispatch(car.ps);
            print(result, True, False);
         end if;
         t := cons_table(t.ps).cdr;
      end loop;
      if t.kind /= E_NIL then
         print(t, False, True);
      end if;
      return NIL_ELEM;
   end;
   --
   function eval_if(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Condition
      p2 : element_type; --  True expression
      p3 : element_type; --  False expression
   begin
      if e.kind /= E_CONS then
         error("eval_if", "Internal error.  Should have a list.");
         return NIL_ELEM;
      end if;
      BBS.lisp.utilities.first_value(e, p1, t);
      if t.kind = E_CONS then
         p2 := cons_table(t.ps).car;
         t := cons_table(t.ps).cdr;
         if t.kind = E_CONS then
            p3 := cons_table(t.ps).car;
         else
            p3 := t;
         end if;
      else
         p2 := t;
         p3 := NIL_ELEM;
      end if;
      --
      --  Now p1 contains the results of evaluating the condition, p2 the
      --  "then" branch, and p3 the "else" branch.  Decide which of p2 or p3
      --  to evaluate.
      --
      t := NIL_ELEM;
      if BBS.lisp.utilities.isTrue(p1) then
         if BBS.lisp.utilities.isFunction(p2) then
            t := eval_dispatch(p2.ps);
         else
            t := BBS.lisp.utilities.indirect_elem(p2);
         end if;
      else
         if BBS.lisp.utilities.isFunction(p3) then
            t := eval_dispatch(p3.ps);
         else
            t := BBS.lisp.utilities.indirect_elem(p3);
         end if;
      end if;
      BBS.lisp.memory.deref(p1);
      return t;
   end;
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   function dowhile(e : element_type) return element_type is
      cond : element_type; --  Condition to evaluate
      list : element_type; --  List of operations to execute
      ptr : element_type;
      t : element_type := NIL_ELEM;
      temp : element_type;
   begin
      if e.kind = E_CONS then
         cond := cons_table(e.ps).car;
         list := cons_table(e.ps).cdr;
         --
         --  Loop while the conditions is true.
         --
         temp := eval_dispatch(cond.ps);
         while bbs.lisp.utilities.isTrue(temp) loop
            BBS.lisp.memory.deref(temp);
            ptr := list;
            --
            --  Evaluate all of the items in the list.
            --
            while ptr.kind /= E_NIL loop
               BBS.lisp.memory.deref(t);
               if ptr.kind = E_CONS then
                  if cons_table(ptr.ps).car.kind = E_CONS then
                     t := eval_dispatch(cons_table(ptr.ps).car.ps);
                  else
                     t := cons_table(ptr.ps).car;
                     BBS.lisp.memory.ref(t);
                  end if;
                  ptr := cons_table(ptr.ps).cdr;
               else
                  t := ptr;
                  BBS.lisp.memory.ref(t);
                  ptr := NIL_ELEM;
               end if;
            end loop;
            temp := eval_dispatch(cond.ps);
         end loop;
         BBS.lisp.memory.deref(temp);
      else
         error("dowhile", "Must provide a condition and expressions.");
      end if;
      return t;
   end;
   --
   function dotimes(e : element_type) return element_type is
   begin
      return NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(e : element_type) return element_type is
   begin
      exit_flag := True;
      return NIL_ELEM;
   end;
   --
   --  Defines a function.  The command is (defun name (parameters) body).
   --    name is a symbol of type LAMBDA.
   --    params is a list of the parameters for the function.  It must be a
   --      list of atoms that translate to symbols or tempsyms.  Defun translates
   --      these to parameter atoms.
   --    body is a list of the actions for the function.  This needs to be
   --      scanned and any symbol or tempsym that matches one of the params is
   --      translated to point to the parameter atom in the parameter list.  It
   --      also could concievable be a single atom or even NIL.
   --
   function defun(e : element_type) return element_type is
      params : element_type;
      func_body : element_type;
      name : element_type;
      temp : element_type;
      symb : symb_index;
      tempsym : tempsym_index;
      flag : Boolean;
      count : Natural := 0;
   begin
      --
      --  First identify the name, parameter list, and body.  Then perform
      --  initial checks to verify that they are the appropriate kind of object.
      --
      if e.kind /= E_CONS then
         error("defun", "No parameters given to defun.");
         return NIL_ELEM;
      end if;
      name := cons_table(e.ps).car;
      temp := cons_table(e.ps).cdr;
      if temp.kind = E_CONS then
         params := cons_table(temp.ps).car;
         func_body := cons_table(temp.ps).cdr;
      else
         error("defun", "Improper parameters.");
         return NIL_ELEM;
      end if;
      if (name.kind /= E_SYMBOL)
        and (name.kind /= E_TEMPSYM) then
         error("defun", "Function name must be a symbol or tempsym.");
         return NIL_ELEM;
      end if;
      if params.kind /= E_CONS then
         error("defun", "Parameter list must be a list.");
         return NIL_ELEM;
      end if;
      --
      --  Second, check the parameter list and convert to parameters.
      --
      temp := params;
      while temp.kind = E_CONS loop
         if cons_table(temp.ps).car.kind = E_CONS then
            error("defun", "A parameter cannot be a list.");
            return NIL_ELEM;
         end if;
         declare
            el : element_type := cons_table(temp.ps).car;
            str : string_index;
         begin
            if (el.kind = E_SYMBOL) then
               str := symb_table(el.sym).str;
               msg("defun", "Converting symbol to parameter");
               el := (kind => E_PARAM, p_name => str,
                      P_value => (kind => V_INTEGER, i => 0));
               cons_table(temp.ps).car := el;
            elsif (el.kind = E_TEMPSYM) then
               msg("defun", "Converting tempsym to parameter");
               str := string_index(tempsym_table(el.tempsym));
               el := (kind => E_PARAM, p_name => str,
                      P_value => (kind => V_INTEGER, i => 0));
               cons_table(temp.ps).car := el;
            end if;
         end;
         temp := cons_table(temp.ps).cdr;
      end loop;
      --
      --  Third, check the body and find the parameters.
      --
      if func_body.kind = E_CONS then
         count := BBS.lisp.utilities.replace_syms(func_body.ps, params.ps);
      else
         return NIL_ELEM;
      end if;
      --
      --  Fourth, if all checks pass, attach the parameter list and body to the
      --  symbol.
      --
      if name.kind = E_TEMPSYM then
         tempsym := name.tempsym;
         flag := get_symb(symb, string_index(tempsym_table(tempsym)));
         if not flag then
            error("defun", "Unable to add symbol ");
            return NIL_ELEM;
         end if;
      elsif name.kind = E_SYMBOL then
         symb := name.sym;
      end if;
      if symb_table(symb).kind = BUILTIN then
         error("defun", "Cannot redefine builtin symbols");
      else
         temp := cons_table(e.ps).cdr;
         cons_table(e.ps).cdr := NIL_ELEM;
         symb_table(symb) := (ref => 1, str => symb_table(symb).str,
                              kind => LAMBDA, ps => temp.ps);
         bbs.lisp.memory.ref(temp.ps);
      end if;
      --
      --  Need to also search for the name of the newly defined symbol to account
      --  for recursive funtions.
      --  TODO
      --
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
      t1 : element_type;
      t2 : element_type;
      ret_val : element_type;
      supplied : Integer := 0;
      requested : Integer := 0;
      --
      --  Find all the parameters used in the function and set their values.
      --
      procedure set_params(func : element_type; params : element_type) is
         t1 : element_type := func;
         t2 : element_type := params;
         temp : element_type;
      begin
         while t1.kind = E_CONS loop
            temp := cons_table(t1.ps).car;
            if temp.kind = E_CONS then
               set_params(temp, params);
            elsif temp.kind = E_PARAM then
               --
               --  Search the parameter list to find the value
               --
               while t2.kind = E_CONS loop
                  if temp.p_name = cons_table(t2.ps).car.p_name then
                     cons_table(t1.ps).car.p_value := cons_table(t2.ps).car.p_value;
                     exit;
                  end if;
                  t2 := cons_table(t2.ps).cdr;
               end loop;
               null;
            end if;
            t1 := cons_table(t1.ps).cdr;
         end loop;
      end;
   begin
      params := cons_table(s).car;
      func_body := cons_table(s).cdr;
      if e.kind = E_CONS then
         supplied := bbs.lisp.utilities.count(e.ps);
      end if;
      if params.kind = E_CONS then
         requested := bbs.lisp.utilities.count(params.ps);
      elsif params.kind /= E_CONS then
         requested := 1;
      end if;
      if supplied /= requested then
         error("function", "Parameter count mismatch. "  & Integer'Image(supplied)
              & " elements supplied, " & Integer'Image(requested) & " requested.");
         return NIL_ELEM;
      end if;
      --
      --  Assign parameters to values.  This needs to change to properly assign
      --  values to parameters.
      --
      t1 := e;      --  Supplied parameter values
      t2 := params; --  List of parameter names
      while t1.kind = E_CONS loop
         if cons_table(t1.ps).car.kind = E_VALUE and
           cons_table(t2.ps).car.kind = E_PARAM then
            cons_table(t2.ps).car.p_value := cons_table(t1.ps).car.v;
         end if;
         t1 := cons_table(t1.ps).cdr;
         t2 := cons_table(t2.ps).cdr;
      end loop;
      set_params(func_body, params);
      --
      --  Evaluate the function
      --
      t1 := func_body;
      ret_val := NIL_ELEM;
      while t1.kind = E_CONS loop
         if cons_table(t1.ps).car.kind = E_CONS then
            ret_val := eval_dispatch(cons_table(t1.ps).car.ps);
         end if;
         t1 := cons_table(t1.ps).cdr;
      end loop;
      return ret_val;
   end;
   --
   function read_line(e : element_type) return element_type is
      buff : String(1 .. 256);
      size : Natural;
      ptr : Natural := buff'First;
      start : Natural := ptr;
      str  : string_index;
      next : string_index;
      first : string_index;
      flag : Boolean;
   begin
      Get_Line(buff, size);
      flag := BBS.lisp.memory.alloc(str);
      if flag then
         string_table(str).len := 0;
         string_table(str).next := -1;
         first := str;
         while (ptr <= size) loop
            if string_table(str).len < fragment_len then
               string_table(str).len := string_table(str).len + 1;
               string_table(str).str(string_table(str).len) := buff(ptr);
            else
               flag := bbs.lisp.memory.alloc(next);
               if flag then
                  string_table(str).next := Integer(next);
                  str := next;
                  string_table(str).len := 1;
                  string_table(str).str(1) := buff(ptr);
                  string_table(str).next := -1;
               else
                  bbs.lisp.memory.deref(first);
                  return NIL_ELEM;
               end if;
            end if;
            ptr := ptr + 1;
         end loop;
      end if;
      ptr := ptr + 1;
      return (kind => E_VALUE, v => (kind => V_STRING, s => first));
   end;
end;
