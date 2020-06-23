with BBS.lisp.parser;
with BBS.lisp.evaluate;
with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.evaluate;
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
      add_builtin("setq", BBS.lisp.evaluate.setq'Access);
      add_builtin("if", BBS.lisp.evaluate.eval_if'Access);
      add_builtin("dowhile", BBS.lisp.evaluate.dowhile'Access);
      add_builtin("dotimes", BBS.lisp.evaluate.dotimes'Access);
      add_builtin("defun", BBS.lisp.evaluate.defun'Access);
      add_builtin("+", BBS.lisp.evaluate.add'Access);
      add_builtin("-", BBS.lisp.evaluate.sub'Access);
      add_builtin("*", BBS.lisp.evaluate.mul'Access);
      add_builtin("/", BBS.lisp.evaluate.div'Access);
      add_builtin("exit", BBS.lisp.evaluate.quit'Access);
      add_builtin("=", BBS.lisp.evaluate.eq'Access);
      add_builtin("/=", BBS.lisp.evaluate.ne'Access);
      add_builtin("<", BBS.lisp.evaluate.lt'Access);
      add_builtin(">", BBS.lisp.evaluate.gt'Access);
      add_builtin("dump", BBS.lisp.evaluate.dump'Access);
      add_builtin("t", BBS.lisp.evaluate.true'Access);
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
      init;
      io_put_line := p_put_line;
      io_put      := p_put;
      io_new_line := p_new_line;
      io_get_line := p_get_line;
   end;
   --
   --  Replacements for Text_IO to make porting to embedded systems easier.
   --  When on a system without Ada.Text_IO, these will need to be changed to
   --  whatever routines are used.
   --
   procedure put_line(s : String) is
   begin
      io_Put_Line.all(s);
   end;
   --
   procedure put(s : String) is
   begin
      io_Put.all(s);
   end;
   --
   procedure new_line is
   begin
      io_New_Line.all;
   end;
   --
   procedure Get_Line(Item : out String; Last : out Natural) is
   begin
      io_Get_Line.all(Item, Last);
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
      bbs.lisp.memory.reset_tempsym;
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
            put("<-");
            print(e.p_value);
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
            put(Boolean'Image(v.b));
         when others =>
            Put("<Unknown value kind>");
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
         when others =>
            Put("<Unknown value kind>");
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
      next : Integer := Integer(s);
      nxt : string_index;
   begin
      while (next >= Integer(string_index'First))
        and (next <= Integer(string_index'Last)) loop
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
   procedure dump_tempsym is
   begin
      for i in tempsym_index loop
         if (tempsym_table(i) >= Integer(string_index'First))
           and (tempsym_table(i) <= Integer(string_index'Last)) then
            Put("Temp Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print(string_index(tempsym_table(i)));
            New_Line;
         end if;
      end loop;
   end;
   --
   --  For debugging, dump all strings
   --
   procedure dump_strings is
   begin
      for i in string_index loop
         if string_table(i).ref > 0 then
            Put("String " & Integer'Image(Integer(i)) & " contains: <"
                                 & string_table(i).str & ">, ");
            Put("Reference count: " & Integer'Image(string_table(i).ref));
            Put(", Length: " & Integer'Image(Integer(string_table(i).len)));
            Put_Line(", Next: " & Integer'Image(string_table(i).next));
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
      for i in symb_index loop
         BBS.lisp.strings.uppercase(n);
         if bbs.lisp.strings.compare(n, symb_table(i).str) = CMP_EQ then
            s := i;
            return true;
         end if;
      end loop;
      s := 0;
      return False;
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
   --  If a temporary symbol exists, return it, otherwise create a new temporary
   --  symbol.  Returns false if symbol doesn't exist and can't be created.
   --
   function get_tempsym(s : out tempsym_index; n : String) return Boolean is
      free : tempsym_index;
      available : Boolean := False;
      name : string_index;
      flag : Boolean;
   begin
      flag := BBS.lisp.strings.str_to_lisp(name, n);
      if flag then
         for i in tempsym_index loop
            if (tempsym_table(i) < Integer(string_index'First))
              or (tempsym_table(i) > Integer(string_index'Last)) then
               free := i;
               available := True;
            elsif bbs.lisp.strings.compare(name, string_index(tempsym_table(i))) = CMP_EQ then
               s := i;
               return true;
            end if;
         end loop;
         if available then
            s := free;
            tempsym_table(s) := Integer(name);
            return True;
         end if;
         error("get_tempsym", "Unable to find empty tempsym");
      else
         error("get_tempsym", "Unable to allocate symbol name.");
      end if;
      s := 0;
      return False;
   end;
   --
   function get_tempsym(s : out tempsym_index; n : string_index) return Boolean is
      free : tempsym_index;
      available : Boolean := False;
   begin
      for i in tempsym_index loop
         if (tempsym_table(i) < Integer(string_index'First))
           or (tempsym_table(i) > Integer(string_index'Last)) then
            free := i;
            available := True;
         elsif bbs.lisp.strings.compare(n, string_index(tempsym_table(i))) = CMP_EQ then
            s := i;
            return true;
         end if;
      end loop;
      if available then
         s := free;
         tempsym_table(s) := Integer(n);
         return True;
      end if;
      error("get_tempsym", "Unable to find empty tempsym");
      s := 0;
      return False;
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
   begin
      if first.kind /= E_CONS then
         if first.kind = E_SYMBOL then
            sym := symb_table(first.sym);
            --
            --  Handle the builtin operations
            --
            if sym.kind = BUILTIN then
               if msg_flag then
                  Put("Evaluating builtin ");
                  Print(sym.str);
                  New_Line;
               end if;
               e := sym.f.all(rest);
            --
            -- Handle defined functions
            --
            elsif sym.kind = LAMBDA then
               if msg_flag then
                  Put("Evaluating lambda ");
                  print(sym.ps);
                  new_line;
               end if;
               e := bbs.lisp.evaluate.eval_function(sym.ps, rest);
            --
            -- Handle variables
            --
            elsif sym.kind = VARIABLE then
               BBS.lisp.memory.ref(sym.pv);
               e := sym.pv;
            end if;
         else -- Not a symbol, just return the value.
            msg("eval_dispatch", "Returning value.");
            dump_cons;
            e := (kind => E_CONS, ps => s);
         end if;
      else -- Not an atom, just return the value
         bbs.lisp.memory.ref(s);
         e := (kind => E_CONS, ps => s);
      end if;
      return e;
   end;


end bbs.lisp;
