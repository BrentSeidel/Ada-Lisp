with Ada.Text_IO;
with Ada.Characters;
with Ada.Characters.Handling;
with BBS.lisp.parser;
with BBS.lisp.evaluate;
with BBS.lisp.memory;
with BBS.lisp.strings;
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
      add_builtin("car", CAR);
      add_builtin("cdr", CDR);
      add_builtin("print", PRINT);
      add_builtin("setq", SETQ);
      add_builtin("=", SYM_EQ);
      add_builtin("/=", SYM_NE);
      add_builtin("<", SYM_LT);
      add_builtin(">", SYM_GT);
      add_builtin("+", PLUS);
      add_builtin("-", MINUS);
      add_builtin("*", MUL);
      add_builtin("/", DIV);
      add_builtin("exit", QUIT_LISP);
      add_builtin("dump", DUMP);
      add_builtin("reset", RESET);
      add_builtin("t", SYM_TRUE);
      add_builtin("if", SYM_IF);
      add_builtin("cond", SYM_COND);
      add_builtin("defun", DEFUN);
      add_builtin("print", PRINT);
      add_builtin("dowhile", DOWHILE);
      add_builtin("dotimes", DOTIMES);
      add_builtin("quote", QUOTE);
      add_builtin("eval", SYM_EVAL);
      add_builtin("new-line", NEWLINE);
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
      Ada.Text_IO.Put("LISP> ");
      Ada.Text_IO.Get_Line(buff, size);
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
      if e.kind = ATOM_TYPE then
         if atom_table(e.pa).kind = ATOM_SYMBOL then
            sym := atom_table(e.pa).sym;
            if symb_table(sym).kind = VARIABLE then
               r := symb_table(sym).pv;
            else
               r := e;
            end if;
         else
            r := e;
         end if;
      elsif e.kind = NIL_TYPE then
         r := e;
      else
         s := e.ps;
         r := bbs.lisp.evaluate.eval_dispatch(s);
         bbs.lisp.memory.deref(s);
      end if;
      return r;
   end;
   --
   --  Prints whatever is pointed to by an element pointer.  If d is true,
   --  the element will be dereffed after printing.  If nl is true, a new
   --  line will be printed at the end.
   --
   procedure print(e : element_type; d : Boolean; nl : Boolean) is
   begin
      if e.kind = CONS_TYPE then
         print(e.ps);
      elsif e.kind = ATOM_TYPE then
         print(atom_table(e.pa));
      elsif e.kind = NIL_TYPE then
         Ada.Text_IO.Put(" NIL");
      else
         Ada.Text_IO.Put("Tried to print an unknown element type.");
      end if;
      if nl then
         Ada.Text_IO.New_Line;
      end if;
      if d then
         bbs.lisp.memory.deref(e);
      end if;
   end;
   --
   procedure dump(e : element_type) is
   begin
      if e.kind = CONS_TYPE then
         dump(e.ps);
      elsif e.kind = ATOM_TYPE then
         dump(atom_table(e.pa));
      elsif e.kind = NIL_TYPE then
         Ada.Text_IO.Put(" NIL");
      else
         Ada.Text_IO.Put("Tried to dump an unknown element type.");
      end if;
   end;
   --
   --  This procedure print a S-expression.
   --
   procedure print(s : cons_index) is
      temp : element_type;
   begin
      Ada.Text_IO.Put("(");
      temp := (kind => CONS_TYPE, ps => s);
      while temp.kind /= NIL_TYPE loop
         if temp.kind = ATOM_TYPE then
            print(atom_table(temp.pa));
            temp := NIL_ELEM;
         elsif temp.kind = CONS_TYPE then
            if cons_table(temp.ps).car.kind = ATOM_TYPE then
               print(atom_table(cons_table(temp.ps).car.pa));
            elsif cons_table(temp.ps).car.kind = CONS_TYPE then
               print(cons_table(temp.ps).car.ps);
            end if;
            temp := cons_table(temp.ps).cdr;
         end if;
      end loop;
      Ada.Text_IO.put(")");
   end;
   --
   procedure dump(s : cons_index) is
      temp : element_type;
   begin
      Ada.Text_IO.Put("(");
      temp := (kind => CONS_TYPE, ps => s);
      while temp.kind /= NIL_TYPE loop
         if temp.kind = ATOM_TYPE then
            dump(atom_table(temp.pa));
            temp := NIL_ELEM;
         elsif temp.kind = CONS_TYPE then
            if cons_table(temp.ps).car.kind = ATOM_TYPE then
               dump(atom_table(cons_table(temp.ps).car.pa));
            elsif cons_table(temp.ps).car.kind = CONS_TYPE then
               dump(cons_table(temp.ps).car.ps);
            end if;
            temp := cons_table(temp.ps).cdr;
         end if;
      end loop;
      Ada.Text_IO.put(")");
   end;
   --
   procedure print(a : atom_index) is
   begin
      print(atom_table(a));
   end;
   --
   procedure dump(a : atom_index) is
   begin
      dump(atom_table(a));
   end;
   --
   procedure print(a : atom) is
   begin
      case a.kind is
         when ATOM_INTEGER =>
            Ada.Text_IO.Put(Integer'Image(a.i) & " ");
         when ATOM_NIL =>
            Ada.Text_IO.Put(" NIL");
         when ATOM_CHARACTER =>
            Ada.Text_IO.Put("'" & a.c & "'");
         when ATOM_SYMBOL =>
            print(a.sym);
         when ATOM_TEMPSYM =>
            if (tempsym_table(a.tempsym) >= Integer(string_index'First)) and
              (tempsym_table(a.tempsym) <= Integer(string_index'Last)) then
               print(string_index(tempsym_table(a.tempsym)));
            else
               Ada.Text_IO.Put("<unallocated tempsym>");
            end if;
         when ATOM_STRING =>
            print(a.str);
         when ATOM_PARAM =>
            print(a.p_name);
         when ATOM_LOCAL =>
            print(a.l_name);
         when others =>
            Ada.Text_IO.Put("<Unknown atom kind>");
      end case;
   end;
   --
   procedure dump(a : atom) is
   begin
      Ada.Text_IO.Put(" Ref count " & Integer'Image(a.ref) & " contains: <");
      case a.kind is
         when ATOM_INTEGER =>
            Ada.Text_IO.Put(Integer'Image(a.i) & " ");
         when ATOM_NIL =>
            Ada.Text_IO.Put(" NIL");
         when ATOM_CHARACTER =>
            Ada.Text_IO.Put("'" & a.c & "'");
         when ATOM_SYMBOL =>
            Ada.Text_IO.Put("<symbol>");
            dump(a.sym);
         when ATOM_TEMPSYM =>
            Ada.Text_IO.Put("<tempsym>");
            if (tempsym_table(a.tempsym) >= Integer(string_index'First)) and
              (tempsym_table(a.tempsym) <= Integer(string_index'Last)) then
               print(string_index(tempsym_table(a.tempsym)));
            else
               Ada.Text_IO.Put("<unallocated tempsym>");
            end if;
         when ATOM_STRING =>
            print(a.str);
         when ATOM_PARAM =>
            Ada.Text_IO.Put("<parameter>");
            print(a.p_name);
            Ada.Text_IO.Put(", <value>");
            print(a.p_value, False, False);
         when ATOM_LOCAL =>
            Ada.Text_IO.Put("<local>");
            print(a.l_name);
            Ada.Text_IO.Put(", <value>");
            print(a.l_value, False, False);
         when others =>
            Ada.Text_IO.Put("<Unknown atom kind>");
      end case;
      Ada.Text_IO.Put_Line(">");
   end;
   --
   --  Print a symbol (BUILTIN, LAMBDA, VARIABLE, EMPTY)
   --
   procedure print(s : symb_index) is
   begin
      print(symb_table(s).str);
      Ada.Text_IO.Put(" ");
   end;
   --
   procedure dump(s : symb_index) is
   begin
      print(symb_table(s).str);
      case symb_table(s).kind is
         when BUILTIN =>
            Ada.Text_IO.Put(" <BUILTIN>");
         when LAMBDA =>
            Ada.Text_IO.Put(" <FUNCTION>");
         when VARIABLE =>
            dump(symb_table(s).pv);
         when others =>
            Ada.Text_IO.Put(" <UNKNOWN>");
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
         Ada.Text_IO.Put(string_table(nxt).str(1..string_table(nxt).len));
         next := string_table(nxt).next;
      end loop;
   end;
   --
   function exit_lisp return Boolean is
   begin
      return exit_flag;
   end;
   --
   --  Private functions and procedures
   --
   --
   --  For debugging, dump all atoms
   --
   procedure dump_atoms is
   begin
      for i in atom_index loop
         if atom_table(i).ref > 0 then
            Ada.Text_IO.Put("Atom " & Integer'Image(Integer(i)));
            dump(atom_table(i));
         end if;
      end loop;
   end;
   --
   --  For debugging, dump all cons cells
   --
   procedure dump_cons is
      procedure print_element(e : element_type) is
      begin
         case e.kind is
            when NIL_TYPE =>
               Ada.Text_IO.Put("NIL");
            when ATOM_TYPE =>
               Ada.Text_IO.Put("ATOM:" & Integer'Image(Integer(e.pa)));
            when CONS_TYPE =>
               Ada.Text_IO.Put("CONS:" & Integer'Image(Integer(e.ps)));
         end case;
      end;
   begin
      for i in cons_index loop
         if cons_table(i).ref > 0 then
            Ada.Text_IO.Put("Cons " & Integer'Image(Integer(i)) & " contains: <");
            print_element(cons_table(i).car);
            Ada.Text_IO.Put(" . ");
            print_element(cons_table(i).cdr);
            Ada.Text_IO.Put_Line(">");
         end if;
      end loop;
   end;
   --
   procedure dump_symbols is
   begin
      for i in symb_index loop
         if symb_table(i).ref > 0 then
            Ada.Text_IO.Put("Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print((symb_table(i).str));
            Ada.Text_IO.Put(" contains: <");
            case symb_table(i).kind is
               when BUILTIN =>
                  Ada.Text_IO.Put("Builtin");
               when LAMBDA =>
                  Ada.Text_IO.Put("Lambda");
               when VARIABLE =>
                  Ada.Text_IO.Put("Variable: ");
                  print(symb_table(i).pv, False, False);
               when EMPTY =>
                  Ada.Text_IO.Put("Empty");
               when others =>
                  Ada.Text_IO.Put("Unknown");
            end case;
            Ada.Text_IO.Put_Line(">");
         end if;
      end loop;
   end;
   --
   procedure dump_tempsym is
   begin
      for i in tempsym_index loop
         if (tempsym_table(i) >= Integer(string_index'First))
           and (tempsym_table(i) <= Integer(string_index'Last)) then
            Ada.Text_IO.Put("Temp Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print(string_index(tempsym_table(i)));
            Ada.Text_IO.New_Line;
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
            Ada.Text_IO.Put("String " & Integer'Image(Integer(i)) & " contains: <"
                                 & string_table(i).str & ">, ");
            Ada.Text_IO.Put("Reference count: " & Integer'Image(string_table(i).ref));
            Ada.Text_IO.Put(", Length: " & Integer'Image(Integer(string_table(i).len)));
            Ada.Text_IO.Put_Line(", Next: " & Integer'Image(string_table(i).next));
         end if;
      end loop;
   end;
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
   procedure add_builtin(n : String; b : builtins) is
      sym : symb_index;
      flag : Boolean;
   begin
      flag := get_symb(sym, n);
      if flag then
         symb_table(sym) := (ref => 1, Kind => BUILTIN, i => b,
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
   function atom_to_cons(s : out cons_index; a : atom_index) return Boolean is
      t : cons_index;
      flag : Boolean;
   begin
      flag := bbs.lisp.memory.alloc(t);
      if flag then
         cons_table(t).car := (kind => ATOM_TYPE, pa => a);
         cons_table(t).cdr := NIL_ELEM;
      end if;
      s := t;
      return flag;
   end;
   --
   --  Appends list s2 to the end of list s1.  If the cdr of the last cons cell
   --  is not NIL_TYPE, this will fail and return False.
   --
   function append(s1 : cons_index; s2 : cons_index) return Boolean is
      t : cons_index;
   begin
      t := s1;
      while cons_table(t).cdr.kind /= NIL_TYPE loop
         if cons_table(t).cdr.kind = CONS_TYPE then
            t := cons_table(t).cdr.ps;
         else
            return False;
            end if;
      end loop;
      cons_table(t).cdr := (Kind => CONS_TYPE, ps => s2);
      return True;
   end;
   --
   --  Procedures for printing error and non-error messages.  Pass in string
   --  representing the function name and the message.  This is intended to
   --  make error messages more consistent.
   --
   procedure error(f : String; m : String) is
   begin
      Ada.Text_IO.Put_Line("ERROR: " & f & ": " & m);
   end;
   --
   procedure msg(f : String; m : String) is
   begin
      Ada.Text_IO.Put_Line("MSG: " & f & ": " & m);
   end;


end bbs.lisp;
