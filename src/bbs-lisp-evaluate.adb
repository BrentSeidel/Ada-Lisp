with bbs.lisp.memory;
with bbs.lisp.strings;
with BBS.lisp.utilities;

package body bbs.lisp.evaluate is
   --
   function eval_newline(e : element_type) return element_type is
   begin
      New_Line;
      return NIL_ELEM;
   end;
   --
   function eval_reset(e : element_type) return element_type is
   begin
      init;
      return NIL_ELEM;
   end;
   --
   function eval_quote(e : element_type) return element_type is
      temp : element_type := e;
   begin
      bbs.lisp.memory.ref(temp);
      return temp;
   end;
   --
   function eval_true(e : element_type) return element_type is
      a : atom_index;
      st : symb_index;
      flag : Boolean;
   begin
      flag := find_symb(st, "t");
      flag := bbs.lisp.memory.alloc(a);
      atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => st);
      return (Kind => ATOM_TYPE, pa => a);
   end;
   --
   function eval_dump(e : element_type) return element_type is
   begin
      dump_atoms;
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
      a : atom_index;
      p : cons_index;
      el: element_type;
      temp : element_type;
      flag : Boolean;
      --
      --  Subfunction to do the actual evaluation.
      --
      function process_atom(a : atom_index; accum : Integer; b : mathops) return Integer is
         a1  : atom_index := a;
         e   : element_type;
      begin
         e := bbs.lisp.utilities.indirect_atom(a);
         if e.kind = ATOM_TYPE then
            a1 := e.pa;
         end if;
         if atom_table(a1).kind = ATOM_INTEGER then
            case (b) is
               when PLUS =>
                  return accum + atom_table(a1).i;
               when MUL =>
                  return accum * atom_table(a1).i;
               when MINUS =>
                  return accum - atom_table(a1).i;
               when DIV =>
                  return accum / atom_table(a1).i;
               when others =>
                  error("eval_math.process_atom", "Internal error, unknown math operation");
            end case;
         else
            error("eval_math.process_atom", "Can't process " & atom_kind'Image(atom_table(a).kind));
         end if;
         return accum;
      end;

   begin
      if e.kind = ATOM_TYPE then
         if atom_table(e.pa).kind = ATOM_INTEGER then
            accum := atom_table(e.pa).i;
         end if;
      elsif e.kind = CONS_TYPE then
         p := e.ps;
         if cons_table(p).car.kind = ATOM_TYPE then
            a := cons_table(p).car.pa;
            el := BBS.lisp.utilities.indirect_atom(a);
            if el.kind = ATOM_TYPE then
               a := el.pa;
            end if;
            if atom_table(a).kind = ATOM_INTEGER then
               accum := atom_table(a).i;
            else
               error("eval_math", "Can't process " & atom_kind'Image(atom_table(a).kind));
            end if;
         elsif cons_table(p).car.kind = CONS_TYPE then
            temp := eval_dispatch(cons_table(p).car.ps);
            if temp.kind = ATOM_TYPE then
               a := temp.pa;
               el := bbs.lisp.utilities.indirect_atom(a);
               if el.kind = ATOM_TYPE then
                  a := el.pa;
               end if;
               if atom_table(a).kind = ATOM_INTEGER then
                  accum := atom_table(a).i;
               else
                  error("eval_math", "Can't process " & atom_kind'Image(atom_table(a).kind));
               end if;
            end if;
            bbs.lisp.memory.deref(temp);
         end if;
         if cons_table(p).cdr.kind /= NIL_TYPE then
            p := cons_table(p).cdr.ps;
            loop
               if cons_table(p).car.kind = ATOM_TYPE then
                  accum := process_atom(cons_table(p).car.pa, accum, b);
               elsif cons_table(p).car.kind = CONS_TYPE then
                  temp := eval_dispatch(cons_table(p).car.ps);
                  if temp.kind = ATOM_TYPE then
                     accum := process_atom(temp.pa, accum, b);
                  end if;
                  bbs.lisp.memory.deref(temp);
               end if;
               exit when cons_table(p).cdr.kind /= CONS_TYPE;
               p := cons_table(p).cdr.ps;
            end loop;
            if cons_table(p).cdr.kind = ATOM_TYPE then
               accum := process_atom(cons_table(p).cdr.pa, accum, b);
            end if;
         end if;
      end if;
      flag := bbs.lisp.memory.alloc(a);
      if flag then
         atom_table(a) := (ref => 1, Kind => ATOM_INTEGER, i => accum);
         el := (Kind => ATOM_TYPE, pa => a);
      else
         error("eval_math", "Unable to allocate atom");
         el := NIL_ELEM;
      end if;
      return el;
   end;
   --
   function eval_add(e : element_type) return element_type is
   begin
      return eval_math(e, PLUS);
   end;
   --
   function eval_sub(e : element_type) return element_type is
   begin
      return eval_math(e, MINUS);
   end;
   --
   function eval_mul(e : element_type) return element_type is
   begin
      return eval_math(e, MUL);
   end;
   --
   function eval_div(e : element_type) return element_type is
   begin
      return eval_math(e, DIV);
   end;
   --
   --  Return the first entry in a list (it may be another list).
   --
   function eval_car(e : element_type) return element_type is
      t : element_type;
   begin
      if e.kind = CONS_TYPE then
         t := cons_table(e.ps).car;
         if t.kind = CONS_TYPE then
            bbs.lisp.memory.ref(cons_table(t.ps).car);
            return cons_table(t.ps).car;
         else
            error("eval_car", "Can only CAR a list");
            return NIL_ELEM;
         end if;
      else
         error("eval_car", "Can only CAR a list");
         return NIL_ELEM;
      end if;
   end;
   --
   --  Return the rest of a list
   --
   function eval_cdr(e : element_type) return element_type is
      t : element_type;
   begin
      if e.kind = CONS_TYPE then
         t := cons_table(e.ps).car;
         if t.kind = CONS_TYPE then
            bbs.lisp.memory.ref(cons_table(t.ps).cdr);
            return cons_table(t.ps).cdr;
         else
            error("eval_cdr", "Can only CDR a list");
            return NIL_ELEM;
         end if;
      else
         error("eval_cdr", "Can only CDR a list");
         return NIL_ELEM;
      end if;
   end;
   --
   --  Perform comparison operations.
   --
   function eval_comp(e : element_type; b : compops) return element_type is
      t  : element_type;
      t1 : element_type;
      t2 : element_type;
      a  : atom_index;
      s  : symb_index;
      flag : Boolean;
      i1 : Integer;
      i2 : Integer;
   begin
      if e.kind = CONS_TYPE then
         BBS.lisp.utilities.first_value(e, t1, t);
         if t.kind = CONS_TYPE then
            BBS.lisp.utilities.first_value(t, t2, t);
         else
            error("eval_comp", "Cannot compare a single atom.");
            return NIL_ELEM;
         end if;
      else
         error("eval_comp", "Cannot compare a single atom.");
         return NIL_ELEM;
      end if;
      if (t1.kind = ATOM_TYPE) and (t2.kind = ATOM_TYPE) then
         t := bbs.lisp.utilities.indirect_atom(t1.pa);
         if t.kind = ATOM_TYPE then
            t1 := t;
         end if;
         t := bbs.lisp.utilities.indirect_atom(t2.pa);
         if t.kind = ATOM_TYPE then
            t2 := t;
         end if;
         if atom_table(t1.pa).kind = ATOM_INTEGER and
           atom_table(t2.pa).kind = ATOM_INTEGER then
            flag := find_symb(s, "t");
            flag := bbs.lisp.memory.alloc(a);
            if flag then
               i1 := atom_table(t1.pa).i;
               i2 := atom_table(t2.pa).i;
               BBS.lisp.memory.deref(t1);
               BBS.lisp.memory.deref(t2);
               case b is
               when SYM_EQ =>
                  if i1 = i2 then
                     atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                     return (Kind => ATOM_TYPE, pa => a);
                  end if;
               when SYM_NE =>
                  if i1 /= i2 then
                     atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                     return (Kind => ATOM_TYPE, pa => a);
                  end if;
               when SYM_LT =>
                  if i1 < i2 then
                     atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                     return (Kind => ATOM_TYPE, pa => a);
                  end if;
               when SYM_GT =>
                  if i1 > i2 then
                     atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                     return (Kind => ATOM_TYPE, pa => a);
                  end if;
               when others =>
                  error("eval_comp", "Unknown comparison type.");
               end case;
               BBS.lisp.memory.deref("eval_comp", a);
               return NIL_ELEM;
            else
               error("eval_comp", "Unable to allocate return value.");
               return NIL_ELEM;
            end if;
         elsif atom_table(t1.pa).kind = ATOM_STRING and
           atom_table(t2.pa).kind = ATOM_STRING then
            flag := find_symb(s, "t");
            flag := bbs.lisp.memory.alloc(a);
            if flag then
               declare
                  eq : comparison;
               begin
                  eq := bbs.lisp.strings.compare(atom_table(t1.pa).str,
                                                atom_table(t2.pa).str);
                  case b is
                     when SYM_EQ =>
                        if eq = CMP_EQ then
                           atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                           return (Kind => ATOM_TYPE, pa => a);
                        end if;
                     when SYM_LT =>
                        if eq = CMP_LT then
                           atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                           return (Kind => ATOM_TYPE, pa => a);
                        end if;
                     when SYM_GT =>
                        if eq = CMP_GT then
                           atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                           return (Kind => ATOM_TYPE, pa => a);
                        end if;
                     when SYM_NE =>
                        if eq /= CMP_EQ then
                           atom_table(a) := (ref => 1, Kind => ATOM_SYMBOL, sym => s);
                           return (Kind => ATOM_TYPE, pa => a);
                        end if;
                  when others =>
                     error("eval_comp", "Unknown comparison type.");
                  end case;
               end;
               bbs.lisp.memory.deref("eval_comp", a);
               return NIL_ELEM;
            else
               error("eval_comp", "Unable to allocate return value.");
               return NIL_ELEM;
            end if;
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
   function eval_eq(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_EQ);
   end;
   --
   function eval_ne(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_NE);
   end;
   --
   function eval_lt(e : element_type) return element_type is
   begin
      return eval_comp(e, SYM_LT);
   end;
   --
   function eval_gt(e : element_type) return element_type is
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
   function eval_setq(e : element_type) return element_type is
      symb : symb_index;
      tempsym : tempsym_index;
      a : atom_index;
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
      if e.kind = CONS_TYPE then
         p1 := cons_table(e.ps).car;  --  Should be symbol name
         p2 := cons_table(e.ps).cdr;  --  Should be value to be assigned
         if p1.kind = ATOM_TYPE then
            a := p1.pa;
            if atom_table(a).kind = ATOM_SYMBOL then
               symb := atom_table(a).sym;
            elsif atom_table(a).kind = ATOM_TEMPSYM then
               tempsym := atom_table(a).tempsym;
               flag := get_symb(symb, string_index(tempsym_table(tempsym)));
               if not flag then
                  error("eval_setq", "Unable to add symbol ");
                  return NIL_ELEM;
               end if;
            else
               error("eval_setq", "First parameter is not a symbol or temporary symbol.");
               return NIL_ELEM;
            end if;
            if symb_table(symb).kind = BUILTIN then
               error("eval_setq", "Can't set value of builtin symbol ");
               return NIL_ELEM;
            end if;
         end if;
         --
         --  At this point, p1 should be an atom containing a valid symbol and
         --  symb is the index to that symbol.
         --
         --
         --  Now determine what value to attach to the symbol.
         --
         if p2.kind = CONS_TYPE then
            p3 := cons_table(p2.ps).car;
            if p3.kind = CONS_TYPE then
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
               BBS.lisp.memory.ref(p3);
               BBS.lisp.memory.ref(p3);
               return p3;
            end if;
         elsif p2.kind = ATOM_TYPE then -- Rare, CDR is an atom.
            deref_previous(symb);
            symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                 pv => p2, str => symb_table(symb).str);
            BBS.lisp.memory.ref(p2);
            BBS.lisp.memory.ref(p2);
            return p2;
         else
            error("eval_setq", "Not enough arguments.");
         end if;
      else
         error("eval_setq", "Not enough arguments.");
      end if;
      return NIL_ELEM;
   end;
   --
   --  Print stuff
   --
   function eval_print(e : element_type) return element_type is
      t : element_type := e;
      result : element_type;
      car : element_type;
   begin
      while t.kind = CONS_TYPE loop
         car := cons_table(t.ps).car;
         if car.kind = ATOM_TYPE then
            result := bbs.lisp.utilities.indirect_atom(car.pa);
            print(result, False, False);
         elsif car.kind = CONS_TYPE then
            result := eval_dispatch(car.ps);
            print(result, True, False);
         end if;
         t := cons_table(t.ps).cdr;
      end loop;
      if t.kind = ATOM_TYPE then
         print(t.pa);
      end if;
      return NIL_ELEM;
   end;
   --
   function eval_if(e : element_type) return element_type is
      t : element_type;
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
   begin
      if e.kind /= CONS_TYPE then
         error("eval_if", "Internal error.  Should have a list.");
         return NIL_ELEM;
      end if;
      p1 := cons_table(e.ps).car;
      t := cons_table(e.ps).cdr;
      if t.kind = CONS_TYPE then
         p2 := cons_table(t.ps).car;
         t := cons_table(t.ps).cdr;
         if t.kind = CONS_TYPE then
            p3 := cons_table(t.ps).car;
         else
            p3 := t;
         end if;
      else
         p2 := t;
         p3 := NIL_ELEM;
      end if;
      --
      --  Now p1 contains the condition, p2 the "then" branch, and p3 the "else"
      --  branch.  Evaluate p1 and decide which of p2 or p3 to evaluate.
      --
      if p1.kind = CONS_TYPE then
         t := eval_dispatch(p1.ps);
         if bbs.lisp.utilities.is_true(t) then
            bbs.lisp.memory.deref(t);
            if p2.kind = CONS_TYPE then
               t := eval_dispatch(p2.ps);
            elsif p2.kind = ATOM_TYPE then
               t := p2;
               bbs.lisp.memory.ref(t);
            else
               t := NIL_ELEM;
            end if;
         else
            bbs.lisp.memory.deref(t);
            if p3.kind = CONS_TYPE then
               t := eval_dispatch(p3.ps);
            elsif p3.kind = ATOM_TYPE then
               t := p3;
               bbs.lisp.memory.ref(t);
            else
               t := NIL_ELEM;
            end if;
         end if;
      elsif p1.kind = ATOM_TYPE then
         if atom_table(p1.pa).kind /= ATOM_NIL then
            if p2.kind = CONS_TYPE then
               t := eval_dispatch(p2.ps);
            elsif p2.kind = ATOM_TYPE then
               t := p2;
               bbs.lisp.memory.ref(t);
            else
               t := NIL_ELEM;
            end if;
         else
            if p3.kind = CONS_TYPE then
               t := eval_dispatch(p3.ps);
            elsif p3.kind = ATOM_TYPE then
               t := p3;
               bbs.lisp.memory.ref(t);
            else
               t := NIL_ELEM;
            end if;
         end if;
      end if;
      return t;
   end;
   --
   --  Evaluates a dowhile command.  The first item is the condition.  If the
   --  condition evaluates to true, the rest of the items in the list are
   --  evaluated.  This is repeated until the condition evaluates to false.
   --
   function eval_dowhile(e : element_type) return element_type is
      cond : element_type;
      list : element_type;
      ptr : element_type;
      t : element_type := NIL_ELEM;
      temp : element_type;
   begin
      if e.kind = CONS_TYPE then
         cond := cons_table(e.ps).car;
         list := cons_table(e.ps).cdr;
         --
         --  Loop while the conditions is true.
         --
         temp := eval_dispatch(cond.ps);
         while bbs.lisp.utilities.is_true(temp) loop
            BBS.lisp.memory.deref(temp);
            ptr := list;
            --
            --  Evaluate all of the items in the list.
            --
            while ptr.kind /= NIL_TYPE loop
               BBS.lisp.memory.deref(t);
               if ptr.kind = CONS_TYPE then
                  if cons_table(ptr.ps).car.kind = CONS_TYPE then
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
         error("eval_dowhile", "Must provide a condition and expressions.");
      end if;
      return t;
   end;
   --
   function eval_dotimes(e : element_type) return element_type is
   begin
      return NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   function eval_quit(e : element_type) return element_type is
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
   function eval_defun(e : element_type) return element_type is
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
      if e.kind /= CONS_TYPE then
         error("eval_defun", "No parameters given to defun.");
         return NIL_ELEM;
      end if;
      name := cons_table(e.ps).car;
      temp := cons_table(e.ps).cdr;
      if temp.kind = CONS_TYPE then
         params := cons_table(temp.ps).car;
         func_body := cons_table(temp.ps).cdr;
      else
         error("eval_defun", "Improper parameters.");
         return NIL_ELEM;
      end if;
      if name.kind = ATOM_TYPE then
         if (atom_table(name.pa).kind /= ATOM_SYMBOL)
           and (atom_table(name.pa).kind /= ATOM_TEMPSYM) then
            error("eval_defun", "Function name must be a symbol or tempsym.");
            return NIL_ELEM;
         end if;
      else
         error("eval_defun", "Function name can't be a list.");
         return NIL_ELEM;
      end if;
      if params.kind /= CONS_TYPE then
         error("eval_defun", "Parameter list must be a list.");
         return NIL_ELEM;
      end if;
      --
      --  Second, check the parameter list and convert to parameters.
      --
      temp := params;
      while temp.kind = CONS_TYPE loop
         if cons_table(temp.ps).car.kind /= ATOM_TYPE then
            error("eval_defun", "A parameter cannot be a list.");
            return NIL_ELEM;
         end if;
         declare
            a : atom_index := cons_table(temp.ps).car.pa;
            count : Natural := atom_table(a).ref;
            str : string_index;
         begin
            if (atom_table(a).kind = ATOM_SYMBOL) then
               str := symb_table(atom_table(a).sym).str;
               atom_table(a) := (ref => count, Kind => ATOM_PARAM, p_name => str,
                                 p_value => NIL_ELEM);
            end if;
            if (atom_table(a).kind = ATOM_TEMPSYM) then
               str := string_index(tempsym_table(atom_table(a).tempsym));
               atom_table(a) := (ref => count, Kind => ATOM_PARAM, p_name => str,
                                 p_value => NIL_ELEM);
            end if;
         end;
         temp := cons_table(temp.ps).cdr;
      end loop;
      --
      --  Third, check the body and find the parameters.
      --
      if func_body.kind = CONS_TYPE then
         count := BBS.lisp.utilities.replace_syms(func_body.ps, params.ps);
      else
         return NIL_ELEM;
      end if;
      --
      --  Fourth, if all checks pass, attach the parameter list and body to the
      --  symbol.
      --
      if atom_table(name.pa).kind = ATOM_TEMPSYM then
         tempsym := atom_table(name.pa).tempsym;
         flag := get_symb(symb, string_index(tempsym_table(tempsym)));
         if not flag then
            error("eval_defun", "Unable to add symbol ");
            return NIL_ELEM;
         end if;
      elsif atom_table(name.pa).kind = ATOM_TEMPSYM then
         symb := atom_table(name.pa).sym;
      end if;
      if symb_table(symb).kind = BUILTIN then
         error("eval_defun", "Cannot redefine builtin symbols");
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
   begin
      params := cons_table(s).car;
      func_body := cons_table(s).cdr;
      Put("Parameter list is: ");
      print(params, False, True);
      Put("Function body is: ");
      print(func_body, False, True);
      if e.kind = CONS_TYPE then
         supplied := bbs.lisp.utilities.count(e.ps);
      end if;
      if params.kind = CONS_TYPE then
         requested := bbs.lisp.utilities.count(params.ps);
      elsif params.kind = ATOM_TYPE then
         requested := 1;
      end if;
      if supplied /= requested then
         error("eval_function", "Parameter count mismatch. "  & Integer'Image(supplied)
              & " elements supplied, " & Integer'Image(requested) & " requested.");
         return NIL_ELEM;
      end if;
      --
      --  Assign parameters to values
      --
      t1 := params;
      t2 := e;
      while t2.kind = CONS_TYPE loop
         Put("Setting: ");
         print(cons_table(t1.ps).car, False, False);
         Put(" to: ");
         print(cons_table(t2.ps).car, False, True);
         if cons_table(t1.ps).car.kind = ATOM_TYPE then
            if cons_table(t2.ps).car.kind = ATOM_TYPE then
               atom_table(cons_table(t1.ps).car.pa).p_value := cons_table(t2.ps).car;
               bbs.lisp.memory.ref(cons_table(t1.ps).car);
            else
               ret_val := eval_dispatch(cons_table(t2.ps).car.ps);
               atom_table(cons_table(t1.ps).car.pa).p_value := ret_val;
               bbs.lisp.memory.ref(ret_val);
            end if;
         end if;
         t1 := cons_table(t1.ps).cdr;
         t2 := cons_table(t2.ps).cdr;
      end loop;
      --
      --  Evaluate the function
      --
--      t1 := cons_table(func_body.ps).car;
      t1 := func_body;
      ret_val := NIL_ELEM;
      while t1.kind = CONS_TYPE loop
         if cons_table(t1.ps).car.kind = CONS_TYPE then
            ret_val := eval_dispatch(cons_table(t1.ps).car.ps);
         end if;
         t1 := cons_table(t1.ps).cdr;
      end loop;
      --
      --  Clean up the assigned parameters
      --
--      Ada.Text_IO.Put_Line("Starting cleanup");
--      dump_atoms;
--      t1 := params;
--      while t1.kind = CONS_TYPE loop
--         if cons_table(t1.ps).car.kind = ATOM_TYPE then
--            if atom_table(cons_table(t1.ps).car.pa).kind = ATOM_PARAM then
--               print(atom_table(cons_table(t1.ps).car.pa).p_value, False, True);
--               bbs.lisp.memory.deref(atom_table(cons_table(t1.ps).car.pa).p_value);
--               atom_table(cons_table(t1.ps).car.pa).p_value := NIL_ELEM;
--            end if;
--         end if;
--         t1 := cons_table(t1.ps).cdr;
--      end loop;
--      Ada.Text_IO.Put_Line("Cleanup done");
--      dump_atoms;
      return ret_val;
   end;
   --
end;
