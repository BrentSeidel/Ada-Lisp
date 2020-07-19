with BBS.lisp.parser;
with BBS.lisp.evaluate;
with BBS.lisp.memory;
with BBS.lisp.stack;
use type BBS.lisp.stack.stack_entry_type;
with BBS.lisp.strings;
with BBS.lisp.evaluate;
with BBS.lisp.evaluate.math;
with BBS.lisp.evaluate.cond;
with BBS.lisp.evaluate.loops;
with BBS.lisp.evaluate.func;
--
package body bbs.lisp is
   --
   --  Initialize the data structures used in the lisp interpreter.  It resets'
   --  the tables and adds the builtin operations to the symbol table.
   --
   procedure init is
   begin
      bbs.lisp.memory.reset_tables;
      --
      add_builtin("car", BBS.lisp.evaluate.car'Access);
      add_builtin("cdr", BBS.lisp.evaluate.cdr'Access);
      add_builtin("print", BBS.lisp.evaluate.print'Access);
      add_special("setq", BBS.lisp.evaluate.setq'Access);
      add_builtin("if", BBS.lisp.evaluate.cond.eval_if'Access);
      add_builtin("dowhile", BBS.lisp.evaluate.loops.dowhile'Access);
      add_special("dotimes", BBS.lisp.evaluate.loops.dotimes'Access);
      add_special("defun", BBS.lisp.evaluate.func.defun'Access);
      add_builtin("+", BBS.lisp.evaluate.math.add'Access);
      add_builtin("-", BBS.lisp.evaluate.math.sub'Access);
      add_builtin("*", BBS.lisp.evaluate.math.mul'Access);
      add_builtin("/", BBS.lisp.evaluate.math.div'Access);
      add_builtin("exit", BBS.lisp.evaluate.quit'Access);
      add_builtin("=", BBS.lisp.evaluate.cond.eq'Access);
      add_builtin("/=", BBS.lisp.evaluate.cond.ne'Access);
      add_builtin("<", BBS.lisp.evaluate.cond.lt'Access);
      add_builtin(">", BBS.lisp.evaluate.cond.gt'Access);
      add_builtin("dump", BBS.lisp.evaluate.dump'Access);
      add_builtin("reset", BBS.lisp.evaluate.reset'Access);
      add_builtin("quote", BBS.lisp.evaluate.quote'Access);
      add_builtin("new-line", BBS.lisp.evaluate.newline'Access);
      add_builtin("msg-on", BBS.lisp.evaluate.msg_on'Access);
      add_builtin("msg-off", BBS.lisp.evaluate.msg_off'Access);
      add_builtin("read-line", BBS.lisp.evaluate.read_line'Access);
      --
      --  The following need functions of the proper form created.
      --
--      add_builtin("cond", SYM_COND);
--      add_builtin("eval", SYM_EVAL);
   end;
   --
   --  Do initialization and define text I/O routines
   --
   procedure init(p_put_line : t_put_line; p_put : t_put_line;
                  p_new_line : t_newline; p_get_line : t_get_line) is
   begin
      io_put_line := p_put_line;
      io_put      := p_put;
      io_new_line := p_new_line;
      io_get_line := p_get_line;
      init;
   end;
   --
   --  Replacements for Text_IO to make porting to embedded systems easier.
   --  When on a system without Ada.Text_IO, these will need to be changed to
   --  whatever routines are used.
   --
   procedure put_line(s : String) is
   begin
      io_Put_Line.all(s);
      first_char_flag := True;
   end;
   --
   procedure put(s : String) is
   begin
      io_Put.all(s);
      first_char_flag := False;
   end;
   --
   procedure new_line is
   begin
      io_New_Line.all;
      first_char_flag := True;
   end;
   --
   procedure Get_Line(Item : out String; Last : out Natural) is
   begin
      io_Get_Line.all(Item, Last);
      first_char_flag := True;
   end;
   --
   --  The read procedure reads text from an input device and parses it into
   --  a S-expression.
   --
   --  Right now, it just creates a simple one for test purposes.
   --
   function read return Element_Type is
      buff : String(1 .. 256);
      size : Natural;
      finished : Boolean := False;
      flag : Boolean;
      el : element_type;
   begin
      Put("LISP> ");
      Get_Line(buff, size);
      flag := bbs.lisp.parser.parse(buff, size, el);
      return el;
   end;
   --
   --  This procedure evaluates a S-expression.  After the expression is
   --  evaluated, it is dereffed.
   --
   function  eval(e : element_type) return element_type is
      s : cons_index;
      r : element_type;
      sym : symb_index;
   begin
      case e.kind is
         when E_CONS =>
            s := e.ps;
            r := eval_dispatch(s);
            bbs.lisp.memory.deref(s);
         when E_SYMBOL =>
            sym := e.sym;
            if symb_table(sym).kind = VARIABLE then
               r := symb_table(sym).pv;
            else
               r := e;
            end if;
         when others =>
            r := e;
      end case;
      return r;
   end;
   --
   --  Prints whatever is pointed to by an element pointer.  If d is true,
   --  the element will be dereffed after printing.  If nl is true, a new
   --  line will be printed at the end.
   --
   procedure print(e : element_type; d : Boolean; nl : Boolean) is
   begin
      case e.kind is
         when E_CONS =>
            print(e.ps);
         when E_NIL =>
            put("Nil");
         when E_VALUE =>
            print(e.v);
         when E_SYMBOL =>
            print(e.sym);
         when E_TEMPSYM =>
            put("Tempsym");
         when E_PARAM =>
            print(e.p_name);
         when E_LOCAL =>
            print(e.l_name);
         when others =>
            Put("Tried to print an unknown element type " & ptr_type'Image(e.kind));
      end case;
      if nl then
         New_Line;
      end if;
      if d then
         bbs.lisp.memory.deref(e);
      end if;
   end;
   --
   procedure dump(e : element_type) is
   begin
      if e.kind = E_CONS then
         dump(e.ps);
      elsif e.kind = E_NIL then
         Put(" NIL");
      else
         Put("Tried to print an unknown element type " & ptr_type'Image(e.kind));
      end if;
   end;
   --
   --  This procedure print a S-expression.
   --
   procedure print(s : cons_index) is
      temp : element_type;
   begin
      Put("(");
      temp := (kind => E_CONS, ps => s);
      while temp.kind /= E_NIL loop
         if temp.kind = E_CONS then
            if cons_table(temp.ps).car.kind = E_CONS then
               print(cons_table(temp.ps).car.ps);
            else
               print(cons_table(temp.ps).car, False, False);
            end if;
            temp := cons_table(temp.ps).cdr;
         else
            print(temp, False, False);
            temp := NIL_ELEM;
         end if;
      end loop;
      put(")");
   end;
   --
   procedure dump(s : cons_index) is
      temp : element_type;
   begin
      Put("(");
      temp := (kind => E_CONS, ps => s);
      while temp.kind /= E_NIL loop
         if temp.kind = E_CONS then
            if cons_table(temp.ps).car.kind = E_CONS then
               dump(cons_table(temp.ps).car.ps);
            end if;
            temp := cons_table(temp.ps).cdr;
         else
            temp := NIL_ELEM;
         end if;
      end loop;
      put(")");
   end;
   --
   procedure print(v : value) is
   begin
      case v.kind is
         when V_INTEGER =>
            Put(Integer'Image(v.i) & " ");
         when V_CHARACTER =>
            Put("'" & v.c & "'");
         when V_STRING =>
            print(v.s);
         when V_BOOLEAN =>
            if v.b then
               put(" T");
            else
               put(" NIL");
            end if;
         when V_NONE =>
            put(" Empty");
         when others =>
            Put("<Unknown value kind " & value_type'Image(v.kind) & ">");
      end case;
   end;
   --
   procedure dump(v : value) is
   begin
      case v.kind is
         when V_INTEGER =>
            Put(Integer'Image(v.i) & " ");
         when V_CHARACTER =>
            Put("'" & v.c & "'");
         when V_STRING =>
            print(v.s);
         when V_BOOLEAN =>
            if v.b then
               put(" T");
            else
               put(" NIL");
            end if;
         when others =>
            Put("<Unknown value kind " & value_type'Image(v.kind) & ">");
      end case;
      Put_Line(">");
   end;
   --
   --  Print a symbol (BUILTIN, LAMBDA, VARIABLE, EMPTY)
   --
   procedure print(s : symb_index) is
   begin
      print(symb_table(s).str);
      Put(" ");
   end;
   --
   procedure dump(s : symb_index) is
   begin
      print(symb_table(s).str);
      case symb_table(s).kind is
         when BUILTIN =>
            Put(" <BUILTIN>");
         when SPECIAL =>
            Put(" <SPECIAL>");
         when LAMBDA =>
            Put(" <FUNCTION>");
         when VARIABLE =>
            dump(symb_table(s).pv);
         when others =>
            Put(" <UNKNOWN>");
      end case;
   end;
   --
   --  Procedure to print a string
   --
   procedure print(s : string_index) is
      next : string_index := s;
      nxt : string_index;
   begin
      while (next >= (string_index'First + 1))
        and (next <= string_index'Last) loop
         nxt := string_index(next);
         Put(string_table(nxt).str(1..string_table(nxt).len));
         next := string_table(nxt).next;
      end loop;
   end;
   --
   function exit_lisp return Boolean is
   begin
      return exit_flag;
   end;
   --
   --  For ease of embedding, this implements the full read-evaluate-print loop.
   --
   procedure repl is
      e : element_type;
      r : element_type;
   begin
      exit_flag := False;
      break_flag := false;
      while True loop
         e := read;
         r := eval(e);
         if not first_char_flag then
            new_line;
         end if;
         print(r, True, True);
         exit when exit_lisp;
      end loop;
   end;
   --
   --  Private functions and procedures
   --
   --
   --  For debugging, dump all cons cells
   --
   procedure dump_cons is
   begin
      for i in cons_index loop
         if cons_table(i).ref > 0 then
            Put("Cons " & Integer'Image(Integer(i)) & " ref count " &
                  Integer'Image(Integer(cons_table(i).ref)) & " contains: <");
            print(cons_table(i).car, False, False);
            Put(" . ");
            print(cons_table(i).cdr, False, False);
            Put_Line(">");
         end if;
      end loop;
   end;
   --
   procedure dump_symbols is
   begin
      for i in symb_index loop
         if symb_table(i).ref > 0 then
            Put("Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print((symb_table(i).str));
            Put(" contains: <");
            case symb_table(i).kind is
               when BUILTIN =>
                  Put("Builtin");
               when SPECIAL =>
                  Put("Special");
               when LAMBDA =>
                  Put("Lambda");
               when VARIABLE =>
                  Put("Variable: ");
                  print(symb_table(i).pv, False, False);
               when EMPTY =>
                  Put("Empty");
               when others =>
                  Put("Unknown");
            end case;
            Put_Line(">");
         end if;
      end loop;
   end;
   --
   --  For debugging, dump all strings
   --
   procedure dump_strings is
   begin
      for i in string_index'First + 1 .. string_index'Last loop
         if string_table(i).ref > 0 then
            Put("String " & Integer'Image(Integer(i)) & " contains: <"
                                 & string_table(i).str & ">, ");
            Put("Reference count: " & Integer'Image(string_table(i).ref));
            Put(", Length: " & Integer'Image(Integer(string_table(i).len)));
            Put_Line(", Next: " & string_index'Image(string_table(i).next));
         end if;
      end loop;
   end;
   --
   --  Functions for symbols.
   --
   --  Find a symbol in the symbol table or create a new entry if the name does
   --  not exist.  Return false if name not found and unable to allocate new
   --  symbol entry.  If a new entry is being created, the kind is set to EMPTY.
   --
   --  The get_symb() functions will probably be depricated for most uses.
   --
   function get_symb(s : out symb_index; n : String) return Boolean is
      free : symb_index;
      available : Boolean := False;
      temp : string_index;
      flag : Boolean;
   begin
      flag := BBS.lisp.strings.str_to_lisp(temp, n);
      if flag then
         BBS.lisp.strings.uppercase(temp);
         for i in symb_index loop
            if symb_table(i).ref = 0 then
               free := i;
               available := True;
            end if;
            if bbs.lisp.strings.compare(temp, symb_table(i).str) = CMP_EQ then
               s := i;
               return True;
            end if;
         end loop;
         if available then
            s := free;
            symb_table(s) := (ref => 1, kind => EMPTY, str => temp);
            return True;
         end if;
      else
         error("get_symb", "Unable to allocate symbol name.");
      end if;
      s := 0;
      return False;
   end;
   --
   function get_symb(s : out symb_index; n : string_index) return Boolean is
      free : symb_index;
      available : Boolean := False;
   begin
      BBS.lisp.strings.uppercase(n);
      for i in symb_index loop
         if symb_table(i).ref = 0 then
            free := i;
            available := True;
         end if;
         if bbs.lisp.strings.compare(n, symb_table(i).str) = CMP_EQ then
            s := i;
            return True;
         end if;
      end loop;
      if available then
         s := free;
         BBS.lisp.memory.ref(n);
         symb_table(s) := (ref => 1, kind => EMPTY, str => n);
         return True;
      end if;
      s := 0;
      return False;
   end;
   --
   --  Finds a symbol and returns it.  Returns false if symbol can't be found.
   --
   function find_symb(s : out symb_index; n : String) return Boolean is
      name : string_index;
      flag : Boolean;
   begin
      flag := BBS.lisp.strings.str_to_lisp(name, n);
      if flag then
         BBS.lisp.strings.uppercase(name);
         for i in symb_index loop
            if bbs.lisp.strings.compare(name, symb_table(i).str) = CMP_EQ then
               s := i;
               BBS.lisp.memory.deref(name);
               return true;
            end if;
         end loop;
         BBS.lisp.memory.deref(name);
      else
         error("find_symb", "Unable to allocate symbol name.");
      end if;
      s := 0;
      return False;
   end;
   --
   function find_symb(s : out symb_index; n : string_index) return Boolean is
   begin
      BBS.lisp.strings.uppercase(n);
      for i in symb_index loop
         if bbs.lisp.strings.compare(n, symb_table(i).str) = CMP_EQ then
            s := i;
            return true;
         end if;
      end loop;
      s := 0;
      return False;
   end;
   --
   --  1. Search the symbol table to determine if the string matches an existing
   --     symbol.  If it does and the symbol is BUILTIN or SPECIAL, return that.
   --  2. Search the stack frames to see if the name matches a variable on the
   --     stack.  If found, return that.
   --  3. If the name matched a symbol of type LAMBDA, VARIABLE, or EMPTY in step
   --     1, return that.
   --  4. Create a new symbol of type EMPTY in the symbol table, if create is True
   --  5. If create is False, return a tempsym.
   --
   function find_variable(n : string_index; create : Boolean) return element_type is
      free : symb_index;
      available : Boolean := False;
      temp : symb_index;
      symb : symbol;
      offset : stack_index;
      sp : stack_index;
      found : Boolean := False;
      item : BBS.lisp.stack.stack_entry;
   begin
      BBS.lisp.strings.uppercase(n);
      --
      --  Search the symbol table
      --
      for i in symb_index loop
         if bbs.lisp.strings.compare(n, symb_table(i).str) = CMP_EQ then
            temp := i;
            symb := symb_table(temp);
            found := True;
            exit;
         end if;
         if symb_table(i).ref = 0 then
            free := i;
            available := True;
         end if;
      end loop;
      if found then
         if (symb.kind = BUILTIN) or (symb.kind = SPECIAL) then
            return (kind => E_SYMBOL, sym => temp);
         end if;
      end if;
      --
      --  Search stack frames.
      --
      offset := BBS.lisp.stack.find_offset(n, sp);
      if (sp > 0) and (offset > 0) then
         item := BBS.lisp.stack.stack(sp);
         if item.kind = BBS.lisp.stack.ST_PARAM then
            BBS.lisp.memory.ref(item.p_name);
            return (kind => E_PARAM, p_name => item.p_name, p_offset => offset);
         elsif item.kind = BBS.lisp.stack.ST_LOCAL then
            BBS.lisp.memory.ref(item.l_name);
            return (kind => E_LOCAL, l_name => item.l_name, l_offset => offset);
         else
            error("find_variable", "Item on stack is of type " &
                    BBS.lisp.stack.stack_entry_type'Image(BBS.lisp.stack.stack(sp).kind));
         end if;
      end if;
      --
      --  Check if the symbol found earlier was LAMBDA, VARIABLE, or EMPTY.  If
      --  so, then use it.
      --
      if found then
         if (symb.kind = LAMBDA) or (symb.kind = VARIABLE) or (symb.kind = EMPTY) then
            return (kind => E_SYMBOL, sym => temp);
         end if;
      end if;
      --
      --  If nothing is found, at this point, try to create a symbol in the symbol
      --  table and return it.
      --
      if create then
         if available then
            BBS.lisp.memory.ref(n);
            symb_table(free) := (ref => 1, kind => EMPTY, str => n);
            return (kind => E_SYMBOL, sym => free);
         end if;
      else
         BBS.lisp.memory.ref(n);
         return (kind => E_TEMPSYM, tempsym => n);
      end if;
      error("find_variable", "Oddly, no option matched.");
      return NIL_ELEM;
   end;
   --
   procedure add_builtin(n : String; f : execute_function) is
      sym : symb_index;
      flag : Boolean;
   begin
      flag := get_symb(sym, n);
      if flag then
         symb_table(sym) := (ref => 1, Kind => BUILTIN, f => f,
                             str => symb_table(sym).str);
      else
         error("add_builtin", "Unable to add builtin symbol " & n);
      end if;
   end;
   --
   procedure add_special(n : String; f : special_function) is
      sym : symb_index;
      flag : Boolean;
   begin
      flag := get_symb(sym, n);
      if flag then
         symb_table(sym) := (ref => 1, Kind => SPECIAL, s => f,
                             str => symb_table(sym).str);
      else
         error("add_special", "Unable to add special symbol " & n);
      end if;
   end;
   --
   --  Creates a cons cell to hold an atom.  This can then be appended to a list.
   --
   function elem_to_cons(s : out cons_index; e : element_type) return Boolean is
      t : cons_index;
      flag : Boolean;
   begin
      flag := bbs.lisp.memory.alloc(t);
      if flag then
         cons_table(t).car := e;
         cons_table(t).cdr := NIL_ELEM;
      end if;
      s := t;
      return flag;
   end;
   --
   --  Appends list s2 to the end of list s1.  If the cdr of the last cons cell
   --  is not E_NIL, this will fail and return False.
   --
   function append(s1 : cons_index; s2 : cons_index) return Boolean is
      t : cons_index;
   begin
      t := s1;
      while cons_table(t).cdr.kind /= E_NIL loop
         if cons_table(t).cdr.kind = E_CONS then
            t := cons_table(t).cdr.ps;
         else
            return False;
            end if;
      end loop;
      cons_table(t).cdr := (Kind => E_CONS, ps => s2);
      return True;
   end;
   --
   --  Procedures for printing error and non-error messages.  Pass in string
   --  representing the function name and the message.  This is intended to
   --  make error messages more consistent.
   --
   procedure error(f : String; m : String) is
   begin
      Put_Line("ERROR: " & f & ": " & m);
   end;
   --
   procedure msg(f : String; m : String) is
   begin
      if msg_flag then
         Put_Line("MSG: " & f & ": " & m);
      end if;
   end;
   --
   --  This is the basic dispatcher for evaluating expressions.  A list has to
   --  start with a symbol to be considered for evaluation.  Some simple items
   --  are handled in this function.  The rest are passed off to sub-functions.
   --
   function eval_dispatch(s : cons_index) return element_type is
      sym : symbol;
      e : element_type := NIL_ELEM;
      first : element_type := cons_table(s).car;
      rest : element_type := cons_table(s).cdr;
      params : element_type;
   begin
      if first.kind /= E_CONS then
         if first.kind = E_SYMBOL then
            sym := symb_table(first.sym);
            case sym.kind is
               when BUILTIN =>
                  if msg_flag then
                     Put("eval_dispatch: Evaluating builtin ");
                     Print(sym.str);
                     New_Line;
                  end if;
                  e := sym.f.all(rest);
               when SPECIAL =>
                  if msg_flag then
                     Put("eval_dispatch: Evaluating special ");
                     Print(sym.str);
                     New_Line;
                  end if;
                  e := sym.s.all(rest, EXECUTE);
               when LAMBDA =>
                  if msg_flag then
                     Put("eval_dispatch: Evaluating lambda ");
                     print(sym.ps);
                     new_line;
                  end if;
                  e := bbs.lisp.evaluate.func.eval_function(sym.ps, rest);
               when VARIABLE =>
                  if msg_flag then
                     Put("eval_dispatch: Evaluating variable ");
                     print(first.p_name);
                     new_line;
                  end if;
                  BBS.lisp.memory.ref(sym.pv);
                  e := sym.pv;
               when others =>
                  if msg_flag then
                     Put("eval_dispatch: Evaluating unknown ");
                     print(first.p_name);
                     new_line;
                  end if;
                  e := NIL_ELEM;
            end case;
         else -- Not a symbol, just return the value.
            if msg_flag then
               Put("eval_dispatch: Evaluating non-symbol ");
               print(first, False, True);
               new_line;
            end if;
            BBS.lisp.memory.ref(s);
            e := (kind => E_CONS, ps => s);
         end if;
      else -- It a cons, just return the value
         if msg_flag then
            Put("eval_dispatch: Evaluating cons ");
            print(first.ps);
            new_line;
         end if;
         e := (kind => E_CONS, ps => s);
      end if;
      if msg_flag then
         Put("eval_dispatch: Returning value: ");
         print(e, False, True);
         dump_cons;
      end if;
      return e;
   end;


end bbs.lisp;
