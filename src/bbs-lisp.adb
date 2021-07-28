with BBS.lisp.evaluate;
with BBS.lisp.evaluate.func;
with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.parser;
with BBS.lisp.parser.stdio;
with BBS.lisp.stack;
use type BBS.lisp.stack.stack_entry_type;
with BBS.lisp.strings;
with BBS.lisp.symbols;
--
package body BBS.lisp
with Refined_State => (pvt_exit_flag => exit_flag,
                       pvt_break_flag => break_flag,
                       pvt_msg_flag => msg_flag,
                       pvt_first_char_flag => first_char_flag,
                       output_stream => (io_put_line, io_put, io_new_line),
                       input_stream => io_get_line,
                       parse => parse_buff) is
   --
   --  Buffer for keyboard input to parser
   --
   parse_buff : aliased BBS.lisp.parser.stdio.parser_stdio;
   --
   --  Initialize the data structures used in the lisp interpreter.  It resets
   --  the tables and adds the builtin operations to the symbol table.
   --
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
      BBS.lisp.memory.reset_tables;
      parse_buff.init;
--      put_line("init: cons size is " & Integer'Image(cons'Size/8) & " bytes");
--      put_line("init: cons_table size is " & Integer'Image(cons_table'Size/8) & " bytes");
--      put_line("init: element size is " & Integer'Image(element_type'Size/8) & " bytes");
--      put_line("init: value size is " & Integer'Image(value'Size/8) & " bytes");
--      put_line("init: symbol size is " & Integer'Image(BBS.lisp.symbols.symbol'Size/8) & " bytes");
--      put_line("init: fixed symbol size is " & Integer'Image(BBS.lisp.symbols.fixed_symbol'Size/8) & " bytes");
--      put_line("init: symbol body size is " & Integer'Image(BBS.lisp.symbols.sym_body'Size/8) & " bytes");
--  init: cons size is  36 bytes
--  init: cons_table size is  18036 bytes
--  init: element size is  16 bytes
--  init: value size is  12 bytes
--  init: symbol size is  32 bytes
--  init: fixed symbol size is  40 bytes
--  init: symbol body size is  24 bytes
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
   function read return Element_Type is
      dummy : Boolean;
      el : element_type;
   begin
      Put(prompt1);
      parse_buff.get_line;
      dummy := BBS.lisp.parser.parse(parse_buff'Access, el);
      return el;
   end;
   --
   --  This procedure evaluates a S-expression.  After the expression is
   --  evaluated, it is dereffed.
   --
   function eval(e : element_type) return element_type is
      r : element_type;
      sym : symbol_ptr;
   begin
      if BBS.lisp.evaluate.isList(e) then
         r := eval_dispatch(BBS.lisp.evaluate.getList(e));
         BBS.lisp.memory.deref(e);
      elsif (e.kind = E_VALUE) and then (e.v.kind = V_SYMBOL) then
         sym := e.v.sym;
         if BBS.lisp.symbols.get_type(sym) = SY_VARIABLE then
            r := BBS.lisp.symbols.get_value(sym);
         else
            r := e;
         end if;
      else
         r := e;
      end if;
      return r;
   end;
   --
   --  Converts an element to a value.  Any element that cannot be converted
   --  returns a value of V_NONE.
   --
   function element_to_value(e : element_type) return value is
      t : element_type;
   begin
      case e.kind is
         when E_EMPTY =>
            return (kind => V_NONE);
         when E_VALUE =>
            t := BBS.lisp.evaluate.indirect_elem(e);
            if t.kind = E_VALUE then
               return t.v;
            else
               return element_to_value(BBS.lisp.evaluate.indirect_elem(t));
            end if;
      end case;
   end;
   --
   --  Create an error value with the specified error code
   --
   function make_error(err : error_code) return element_type is
   begin
      return (Kind => E_VALUE, v => (kind => V_ERROR, err => err));
   end;
   --
   --  Prints whatever is pointed to by an element pointer.  If d is true,
   --  the element will be dereffed after printing.  If nl is true, a new
   --  line will be printed at the end.
   --
   procedure print(e : element_type; d : Boolean; nl : Boolean) is
   begin
      case e.kind is
         when E_EMPTY =>
            put("<nothing>");
         when E_VALUE =>
            print(e.v);
      end case;
      if nl then
         New_Line;
      end if;
      if d then
         bbs.lisp.memory.deref(e);
      end if;
   end;
   --
   --  This procedure print a S-expression.
   --
   procedure print(s : cons_index) is
      list : cons_index := s;
   begin
      Put("(");
      if s = NIL_CONS then
         put(")");
         return;
      end if;
      while list > NIL_CONS loop
         if BBS.lisp.evaluate.isList(cons_table(list).car) then
            print(BBS.lisp.evaluate.getList(cons_table(list).car));
         else
            print(cons_table(list).car, False, False);
         end if;
         if not BBS.lisp.evaluate.isList(cons_table(list).cdr) and (cons_table(list).cdr /= NIL_ELEM) then
            put(" . ");
            print(cons_table(list).cdr, False, False);
         end if;
         list := BBS.lisp.evaluate.getList(cons_table(list).cdr);
      end loop;
      put(")");
   end;
   --
   procedure print(v : value) is
   begin
      case v.kind is
         when V_INTEGER =>
            Put(int32'Image(v.i));
         when V_CHARACTER =>
            Put("" & v.c);
         when V_STRING =>
            print(v.s);
         when V_BOOLEAN =>
            if v.b then
               put(" T");
            else
               put(" NIL");
            end if;
         when V_LIST =>
            print(v.l);
         when V_LAMBDA =>
            print(v.lam);
         when V_TEMPSYM =>
            put("Tempsym[");
            print(v.tempsym);
            put("]");
         when V_SYMBOL =>
            print(v.sym);
         when V_QSYMBOL =>
            put("'");
            print(v.qsym);
         when V_STACK =>
            print(v.st_name);
         when V_ERROR =>
            put("ERROR: " & error_code'Image(v.err));
         when V_NONE =>
            put(" Nil");
--         when others =>
--            Put("<Unknown value kind " & value_type'Image(v.kind) & ">");
      end case;
   end;
   --
   --  Print a symbol (BUILTIN, LAMBDA, VARIABLE, EMPTY)
   --
   procedure print(s : symbol_ptr) is
   begin
      if s.kind = ST_FIXED then
         put(BBS.lisp.symbols.get_name(s).all);
      else
         print(BBS.lisp.symbols.get_name(s));
      end if;
      Put(" ");
   end;
   --
   --  Procedure to print a string
   --
   procedure print(s : string_index) is
   begin
      BBS.lisp.strings.print(s);
   end;
   --
   --  Dump a symbol pointer.
   --
   procedure dump_sym_ptr(s : symbol_ptr) is
   begin
      case s.kind is
         when ST_NULL =>
            put_line("<Null symbol>");
         when ST_FIXED =>
            put_line("Fixed: " & fsymb_index'Image(s.f) & ", name <" &
                       BBS.lisp.symbols.get_name(s).all & ">");
         when ST_DYNAMIC =>
            put("Dynamic: " & symb_index'Image(s.d) & ", name <");
            print(BBS.lisp.symbols.get_name(s));
            put_line(">");
      end case;
   end;
   --
   --  Return the exit flag.
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
         BBS.lisp.evaluate.set_exit_block(0);
         e := read;
         if not ((e.kind = E_VALUE) and then (e.v.kind = V_ERROR)) then
            r := eval(e);
            if not first_char_flag then
               new_line;
            end if;
            print(r, True, True);
         end if;
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
      for i in cons_index'First + 1 .. cons_index'Last loop
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
      ptr : symbol_ptr;
   begin
      for i in symb_index'First + 1 .. symb_index'Last loop
         ptr := (kind => ST_DYNAMIC, d => i);
         if BBS.lisp.symbols.get_ref(ptr) > 0 then
            Put("Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print(BBS.lisp.symbols.get_name(ptr));
            Put(" contains: <");
            case BBS.lisp.symbols.get_type(ptr) is
               when SY_BUILTIN =>
                  Put("Builtin");
               when SY_SPECIAL =>
                  Put("Special");
               when SY_VARIABLE =>
                  Put("Variable: ");
                  print(BBS.lisp.symbols.get_value(ptr), False, False);
               when SY_EMPTY =>
                  Put("Empty");
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
      BBS.lisp.strings.dump_strings;
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
   function get_symb(s : out symbol_ptr; n : String) return Boolean is
      temp : string_index;
      flag : Boolean;
   begin
      flag := BBS.lisp.strings.str_to_lisp(temp, n);
      if flag then
         return get_symb(s, temp);
      else
         error("get_symb", "Unable to allocate symbol name.");
      end if;
      s := (kind => ST_NULL);
      return False;
   end;
   --
   function get_symb(s : out symbol_ptr; n : string_index) return Boolean is
      free : symb_index;
      fixed : symbol_ptr;
      available : Boolean := False;
   begin
      BBS.lisp.strings.uppercase(n);
      fixed := BBS.lisp.symbols.find_name(n);
      if fixed.kind = ST_FIXED then
         s := fixed;
         return True;
      end if;
      for i in symb_index'First + 1 .. symb_index'Last loop
         if BBS.lisp.symbols.get_ref((kind => ST_DYNAMIC, d => i)) = 0 then
            free := i;
            available := True;
         else
            if bbs.lisp.strings.compare(n, BBS.lisp.symbols.get_name((kind => ST_DYNAMIC, d => i))) = CMP_EQ then
               s := (kind => ST_DYNAMIC, d => i);
               return True;
            end if;
         end if;
      end loop;
      if available then
         s := (kind => ST_DYNAMIC, d => free);
         BBS.lisp.strings.ref(n);
         BBS.lisp.symbols.add_sym(s, (ref => 1, name => n, b => (kind => SY_EMPTY)));
         return True;
      end if;
      s := (kind => ST_NULL);
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
      free : symbol_ptr;
      fsym : symbol_ptr;
      available : Boolean := False;
      temp : symb_index;
      symb : BBS.lisp.symbols.sym_body;
      offset : Natural;
      sp : Natural;
      found : Boolean := False;
      item : BBS.lisp.stack.stack_entry;
      err : Boolean;
   begin
      --
      --  Check the fixed symbol table
      --
      fsym := BBS.lisp.symbols.find_name(n);
      if fsym.kind = ST_FIXED then
         if BBS.lisp.symbols.get_type(fsym) = SY_VARIABLE then
            return BBS.lisp.symbols.get_value(fsym);
         else
            return (kind => E_VALUE, v => (Kind => V_SYMBOL, sym => fsym));
         end if;
      end if;
      --
      --  Search the symbol table
      --
      for i in symb_index'First + 1 .. symb_index'Last loop
         if BBS.lisp.symbols.get_ref((kind => ST_DYNAMIC, d => i)) = 0 then
            free := (kind => ST_DYNAMIC, d => i);
            available := True;
         else
            if bbs.lisp.strings.compare(n, BBS.lisp.symbols.get_name((kind => ST_DYNAMIC, d => i))) = CMP_EQ then
               temp := i;
               symb := BBS.lisp.symbols.get_sym((kind => ST_DYNAMIC, d => temp));
               found := True;
               exit;
            end if;
         end if;
      end loop;
      if found then
         if (symb.kind = SY_BUILTIN) or (symb.kind = SY_SPECIAL) then
            return (kind => E_VALUE, v => (kind => V_SYMBOL, sym => (kind => ST_DYNAMIC, d => temp)));
         end if;
      end if;
      --
      --  Search stack frames.
      --
      offset := BBS.lisp.global.stack.find_offset(n, sp);
      if offset > Natural'First then
         item := BBS.lisp.global.stack.get_entry(sp, err);
         if item.kind = BBS.lisp.stack.ST_VALUE then
            BBS.lisp.strings.ref(item.st_name);
            return (kind => E_VALUE, v => (kind => V_STACK, st_name => item.st_name, st_offset => offset));
         else
            error("find_variable", "Item on stack is of type " &
                    BBS.lisp.stack.stack_entry_type'Image(BBS.lisp.global.stack.get_entry(sp, err).kind));
         end if;
      end if;
      --
      --  Check if the symbol found earlier was LAMBDA, VARIABLE, or EMPTY.  If
      --  so, then use it.
      --
      if found then
         if (symb.kind = SY_VARIABLE) or (symb.kind = SY_EMPTY) then
            return (kind => E_VALUE, v => (kind => V_SYMBOL, sym => (kind => ST_DYNAMIC, d => temp)));
         end if;
      end if;
      --
      --  If nothing is found, at this point, try to create a symbol in the symbol
      --  table and return it.
      --
      if create then
         if available then
            BBS.lisp.strings.ref(n);
            BBS.lisp.symbols.add_sym(free, (ref => 1, name => n, b => (kind => SY_EMPTY)));
            return (kind => E_VALUE, v => (kind => V_SYMBOL, sym => free));
         end if;
      else
         BBS.lisp.strings.ref(n);
         return (kind => E_VALUE, v=> (kind => V_TEMPSYM, tempsym => n));
      end if;
      error("find_variable", "Oddly, no option matched.");
      return make_error(ERR_UNKNOWN);
   end;
   --
   procedure add_builtin(n : String; f : execute_function) is
      sym : symbol_ptr;
   begin
      if get_symb(sym, n) then
         BBS.lisp.symbols.set_sym(sym, (kind => SY_BUILTIN, f => f));
      else
         error("add_builtin", "Unable to add builtin symbol " & n);
      end if;
   end;
   --
   procedure add_special(n : String; f : special_function) is
      sym : symbol_ptr;
   begin
      if get_symb(sym, n) then
         BBS.lisp.symbols.set_sym(sym, (kind => SY_SPECIAL, s => f));
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
      while cons_table(t).cdr /= NIL_ELEM loop
         t := BBS.lisp.evaluate.getList(cons_table(t).cdr);
         if t = NIL_CONS then
            return False;
         end if;
      end loop;
      cons_table(t).cdr := BBS.lisp.evaluate.makeList(s2);
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
      sym : BBS.lisp.symbols.sym_body;
      sym_flag : Boolean := False;
      e : element_type := NIL_ELEM;
      first : constant element_type := cons_table(s).car;
      rest : constant cons_index := BBS.lisp.evaluate.getList(cons_table(s).cdr);
      val : value;
   begin
      if (first.kind = E_VALUE) and then (first.v.kind = V_SYMBOL) then
         sym := BBS.lisp.symbols.get_sym(first.v.sym);
         sym_flag := True;
      elsif (first.kind = E_VALUE) and then (first.v.kind = V_STACK) then
         val := BBS.lisp.global.stack.search_frames(first.v.st_offset, first.v.st_name);
         if val.kind = V_SYMBOL then
            sym := BBS.lisp.symbols.get_sym(val.sym);
            sym_flag := True;
         end if;
      end if;
      if sym_flag then
         case sym.kind is
            when SY_BUILTIN =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating builtin ");
                  if first.v.sym.kind = ST_FIXED then
                     put(BBS.lisp.symbols.get_name(first.v.sym).all);
                  else
                     Print(BBS.lisp.symbols.get_name(first.v.sym));
                  end if;
                  New_Line;
               end if;
               sym.f.all(e, rest);
            when SY_SPECIAL =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating special ");
                  if first.v.sym.kind = ST_FIXED then
                     put(BBS.lisp.symbols.get_name(first.v.sym).all);
                  else
                     Print(BBS.lisp.symbols.get_name(first.v.sym));
                  end if;
                  New_Line;
               end if;
               sym.s.all(e, rest, PH_EXECUTE);
            when SY_VARIABLE =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating variable ");
                  if first.v.sym.kind = ST_FIXED then
                     put(BBS.lisp.symbols.get_name(first.v.sym).all);
                  else
                     Print(BBS.lisp.symbols.get_name(first.v.sym));
                  end if;
                  new_line;
               end if;
               if (sym.pv.kind = E_VALUE) and then (sym.pv.v.kind = V_LAMBDA) then
                  if msg_flag then
                     Put("eval_dispatch: Evaluating lambda ");
                     print(sym.pv.v);
                     new_line;
                  end if;
                  e := bbs.lisp.evaluate.func.eval_function(sym.pv.v.lam, rest);
               else
                  BBS.lisp.memory.ref(sym.pv);
                  e := sym.pv;
               end if;
            when others =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating unknown ");
                  if first.v.sym.kind = ST_FIXED then
                     put(BBS.lisp.symbols.get_name(first.v.sym).all);
                  else
                     Print(BBS.lisp.symbols.get_name(first.v.sym));
                  end if;
                  new_line;
               end if;
               e := NIL_ELEM;
         end case;
      elsif first.kind = E_VALUE then
         if first.v.kind = V_LAMBDA then
            if msg_flag then
               Put("eval_dispatch: Evaluating lambda ");
               print(first.v.lam);
               new_line;
            end if;
            e := bbs.lisp.evaluate.func.eval_function(first.v.lam, rest);
         elsif first.v.kind = V_STACK then
            val := BBS.lisp.global.stack.search_frames(first.v.st_offset, first.v.st_name);
            if val.kind = V_LAMBDA then
               if msg_flag then
                  Put("eval_dispatch: Evaluating lambda ");
                  print(val);
                  new_line;
               end if;
               e := bbs.lisp.evaluate.func.eval_function(val.lam, rest);
            else
               BBS.lisp.memory.ref(s);
               e := BBS.lisp.evaluate.makeList(s);
            end if;
         else
            BBS.lisp.memory.ref(s);
            e := BBS.lisp.evaluate.makeList(s);
         end if;
      elsif BBS.lisp.evaluate.isList(first) then
         if msg_flag then
            Put("eval_dispatch: Evaluating cons ");
            print(BBS.lisp.evaluate.getList(first));
            new_line;
         end if;
         e := BBS.lisp.evaluate.makeList(s);
      else  --  Not a symbol, just return the value.
         if msg_flag then
            Put("eval_dispatch: Evaluating non-symbol ");
            print(first, False, True);
            new_line;
         end if;
         BBS.lisp.memory.ref(s);
         e := BBS.lisp.evaluate.makeList(s);
      end if;
      if msg_flag then
         Put("eval_dispatch: Returning value: ");
         print(e, False, True);
      end if;
      return e;
   end;
   --
end BBS.lisp;
