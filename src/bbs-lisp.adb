with BBS.lisp.evaluate;
with BBS.lisp.evaluate.bool;
with BBS.lisp.evaluate.char;
with BBS.lisp.evaluate.cond;
with BBS.lisp.evaluate.func;
with BBS.lisp.evaluate.io;
with BBS.lisp.evaluate.list;
with BBS.lisp.evaluate.loops;
with BBS.lisp.evaluate.math;
with BBS.lisp.evaluate.mem;
with BBS.lisp.evaluate.misc;
with BBS.lisp.evaluate.pred;
with BBS.lisp.evaluate.str;
with BBS.lisp.evaluate.symb;
with BBS.lisp.evaluate.vars;
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
   --  Initialize the data structures used in the lisp interpreter.  It resets'
   --  the tables and adds the builtin operations to the symbol table.
   --
   procedure init is
   begin
      bbs.lisp.memory.reset_tables;
      --
      add_builtin("+", BBS.lisp.evaluate.math.add'Access);
      add_builtin("-", BBS.lisp.evaluate.math.sub'Access);
      add_builtin("*", BBS.lisp.evaluate.math.mul'Access);
      add_builtin("/", BBS.lisp.evaluate.math.div'Access);
      add_builtin("=", BBS.lisp.evaluate.cond.eq'Access);
      add_builtin("/=", BBS.lisp.evaluate.cond.ne'Access);
      add_builtin("<", BBS.lisp.evaluate.cond.lt'Access);
      add_builtin(">", BBS.lisp.evaluate.cond.gt'Access);
      add_builtin("and", BBS.lisp.evaluate.bool.eval_and'Access);
      add_builtin("arrayp", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("atomp", BBS.lisp.evaluate.pred.atomp'Access);
      add_builtin("bit-vector-p", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("car", BBS.lisp.evaluate.list.car'Access);
      add_builtin("cdr", BBS.lisp.evaluate.list.cdr'Access);
      add_builtin("char", BBS.lisp.evaluate.str.char'Access);
      add_builtin("char-code", BBS.lisp.evaluate.char.char_code'Access);
      add_builtin("char-downcase", BBS.lisp.evaluate.char.char_downcase'Access);
      add_builtin("char-upcase", BBS.lisp.evaluate.char.char_upcase'Access);
      add_builtin("characterp", BBS.lisp.evaluate.pred.characterp'Access);
      add_builtin("code-char", BBS.lisp.evaluate.char.code_char'Access);
      add_builtin("coerce", BBS.lisp.evaluate.symb.coerce'Access);
      add_builtin("compiled-function-p", BBS.lisp.evaluate.pred.compiled_function_p'Access);
      add_builtin("complexp", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("concatenate", BBS.lisp.evaluate.symb.concatenate'Access);
      add_builtin("cons", BBS.lisp.evaluate.list.cons'Access);
      add_builtin("consp", BBS.lisp.evaluate.pred.consp'Access);
      add_builtin("errorp", BBS.lisp.evaluate.pred.errorp'Access);
      add_special("defun", BBS.lisp.evaluate.func.defun'Access);
      add_special("dolist", BBS.lisp.evaluate.loops.dolist'Access);
      add_special("dotimes", BBS.lisp.evaluate.loops.dotimes'Access);
      add_builtin("dowhile", BBS.lisp.evaluate.loops.dowhile'Access);
      add_builtin("dump", BBS.lisp.evaluate.misc.dump'Access);
      add_builtin("eval", BBS.lisp.evaluate.func.eval_list'Access);
      add_builtin("exit", BBS.lisp.evaluate.misc.quit'Access);
      add_builtin("floatp", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("fresh-line", BBS.lisp.evaluate.io.fresh_line'Access);
      add_builtin("functionp", BBS.lisp.evaluate.pred.functionp'Access);
      add_builtin("if", BBS.lisp.evaluate.cond.eval_if'Access);
      add_builtin("integerp", BBS.lisp.evaluate.pred.integerp'Access);
      add_special("lambda", BBS.lisp.evaluate.func.lambda'Access);
      add_builtin("length", BBS.lisp.evaluate.str.length'Access);
      add_special("let", BBS.lisp.evaluate.vars.local'Access);
      add_builtin("list", BBS.lisp.evaluate.list.list'Access);
      add_builtin("listp", BBS.lisp.evaluate.pred.listp'Access);
      add_builtin("msg", BBS.lisp.evaluate.misc.msg'Access);
      add_builtin("not", BBS.lisp.evaluate.bool.eval_not'Access);
      add_builtin("null", BBS.lisp.evaluate.pred.nullp'Access);
      add_builtin("numberp", BBS.lisp.evaluate.pred.numberp'Access);
      add_builtin("or", BBS.lisp.evaluate.bool.eval_or'Access);
      add_builtin("packagep", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("parse-integer", BBS.lisp.evaluate.str.parse_integer'Access);
      add_builtin("peek8", BBS.lisp.evaluate.mem.peek8'Access);
      add_builtin("peek16", BBS.lisp.evaluate.mem.peek16'Access);
      add_builtin("peek32", BBS.lisp.evaluate.mem.peek32'Access);
      add_builtin("poke8", BBS.lisp.evaluate.mem.poke8'Access);
      add_builtin("poke16", BBS.lisp.evaluate.mem.poke16'Access);
      add_builtin("poke32", BBS.lisp.evaluate.mem.poke32'Access);
      add_builtin("print", BBS.lisp.evaluate.io.print'Access);
      add_builtin("progn", BBS.lisp.evaluate.loops.progn'Access);
      add_builtin("quote", BBS.lisp.evaluate.list.quote'Access);
      add_builtin("rationalp", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("read", BBS.lisp.evaluate.io.read_expr'Access);
      add_builtin("read-line", BBS.lisp.evaluate.io.read_line'Access);
      add_builtin("realp", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("return", BBS.lisp.evaluate.loops.return_from'Access);
      add_special("setq", BBS.lisp.evaluate.vars.setq'Access);
      add_builtin("simple-bit-vector-p", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("simple-string-p", BBS.lisp.evaluate.pred.simple_string_p'Access);
      add_builtin("simple-vector-p", BBS.lisp.evaluate.pred.return_false'Access);
      add_builtin("sleep", BBS.lisp.evaluate.misc.sleep'Access);
      add_builtin("string-downcase", BBS.lisp.evaluate.str.string_downcase'Access);
      add_builtin("string-upcase", BBS.lisp.evaluate.str.string_upcase'Access);
      add_builtin("stringp", BBS.lisp.evaluate.pred.stringp'Access);
      add_builtin("subseq", BBS.lisp.evaluate.str.subseq'Access);
      add_builtin("symbolp", BBS.lisp.evaluate.pred.symbolp'Access);
      add_builtin("terpri", BBS.lisp.evaluate.io.terpri'Access);
      add_builtin("vectorp", BBS.lisp.evaluate.pred.return_false'Access);
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
      parse_buff.init;
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
      sym : symb_index;
   begin
      if BBS.lisp.evaluate.isList(e) then
         r := eval_dispatch(BBS.lisp.evaluate.getList(e));
         BBS.lisp.memory.deref(e);
      elsif e.kind = E_SYMBOL then
         sym := e.sym;
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
   begin
      case e.kind is
         when E_ERROR =>
            return (kind => V_NONE);
         when E_NIL =>
            return (kind => V_NONE);
         when E_TEMPSYM =>
            return (kind => V_NONE);
         when E_SYMBOL =>
            return element_to_value(BBS.lisp.evaluate.indirect_elem(e));
         when E_STACK =>
            return element_to_value(BBS.lisp.evaluate.indirect_elem(e));
         when E_VALUE =>
            return e.v;
      end case;
   end;
   --
   --  Prints whatever is pointed to by an element pointer.  If d is true,
   --  the element will be dereffed after printing.  If nl is true, a new
   --  line will be printed at the end.
   --
   procedure print(e : element_type; d : Boolean; nl : Boolean) is
   begin
      case e.kind is
         when E_ERROR =>
            put("ERROR");
         when E_NIL =>
            put("Nil");
         when E_VALUE =>
            print(e.v);
         when E_SYMBOL =>
            print(e.sym);
         when E_TEMPSYM =>
            put("Tempsym[");
            print(e.tempsym);
            put("]");
         when E_STACK =>
            print(e.st_name);
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
         if not BBS.lisp.evaluate.isList(cons_table(list).cdr) and (cons_table(list).cdr.kind /= E_NIL) then
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
         when V_SYMBOL =>
            print(v.sym);
         when V_QSYMBOL =>
            put("'");
            print(v.qsym);
         when V_NONE =>
            put(" Empty");
--         when others =>
--            Put("<Unknown value kind " & value_type'Image(v.kind) & ">");
      end case;
   end;
   --
   --  Print a symbol (BUILTIN, LAMBDA, VARIABLE, EMPTY)
   --
   procedure print(s : symb_index) is
   begin
      print(BBS.lisp.symbols.get_name(s));
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
         if e.kind /= E_ERROR then
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
   begin
      for i in symb_index'First + 1 .. symb_index'Last loop
         if BBS.lisp.symbols.get_ref(i) > 0 then
            Put("Symbol " & Integer'Image(Integer(i))
                            & " Name ");
            print(BBS.lisp.symbols.get_name(i));
            Put(" contains: <");
            case BBS.lisp.symbols.get_type(i) is
               when SY_BUILTIN =>
                  Put("Builtin");
               when SY_SPECIAL =>
                  Put("Special");
               when SY_LAMBDA =>
                  Put("Lambda");
               when SY_VARIABLE =>
                  Put("Variable: ");
                  print(BBS.lisp.symbols.get_value(i), False, False);
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
   function get_symb(s : out symb_index; n : String) return Boolean is
      temp : string_index;
   begin
      if BBS.lisp.strings.str_to_lisp(temp, n) then
         return get_symb(s, temp);
      else
         error("get_symb", "Unable to allocate symbol name.");
      end if;
      s := NIL_SYM;
      return False;
   end;
   --
   function get_symb(s : out symb_index; n : string_index) return Boolean is
      free : symb_index;
      available : Boolean := False;
   begin
      BBS.lisp.strings.uppercase(n);
      for i in symb_index'First + 1 .. symb_index'Last loop
         if BBS.lisp.symbols.get_ref(i) = 0 then
            free := i;
            available := True;
         else
            if bbs.lisp.strings.compare(n, BBS.lisp.symbols.get_name(i)) = CMP_EQ then
               s := i;
               return True;
            end if;
         end if;
      end loop;
      if available then
         s := free;
         BBS.lisp.strings.ref(n);
         BBS.lisp.symbols.set_sym(s, (ref => 1, kind => SY_EMPTY, name => n, b => (kind => SY_EMPTY)));
         return True;
      end if;
      s := NIL_SYM;
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
      symb : BBS.lisp.symbols.symbol;
      offset : Natural;
      sp : Natural;
      found : Boolean := False;
      item : BBS.lisp.stack.stack_entry;
      err : Boolean;
   begin
      BBS.lisp.strings.uppercase(n);
      --
      --  Search the symbol table
      --
      for i in symb_index'First + 1 .. symb_index'Last loop
         if BBS.lisp.symbols.get_ref(i) = 0 then
            free := i;
            available := True;
         else
            if bbs.lisp.strings.compare(n, BBS.lisp.symbols.get_name(i)) = CMP_EQ then
               temp := i;
               symb := BBS.lisp.symbols.get_sym(temp);
               found := True;
               exit;
            end if;
         end if;
      end loop;
      if found then
         if (symb.kind = SY_BUILTIN) or (symb.kind = SY_SPECIAL) then
            return (kind => E_SYMBOL, sym => temp);
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
            return (kind => E_STACK, st_name => item.st_name, st_offset => offset);
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
         if (symb.kind = SY_LAMBDA) or (symb.kind = SY_VARIABLE) or (symb.kind = SY_EMPTY) then
            return (kind => E_SYMBOL, sym => temp);
         end if;
      end if;
      --
      --  If nothing is found, at this point, try to create a symbol in the symbol
      --  table and return it.
      --
      if create then
         if available then
            BBS.lisp.strings.ref(n);
            BBS.lisp.symbols.add_sym((kind => ST_DYNAMIC, d => free), (ref => 1, kind => SY_EMPTY, name => n, b => (kind => SY_EMPTY)));
            return (kind => E_SYMBOL, sym => free);
         end if;
      else
         BBS.lisp.strings.ref(n);
         return (kind => E_TEMPSYM, tempsym => n);
      end if;
      error("find_variable", "Oddly, no option matched.");
      return (kind => E_ERROR);
   end;
   --
   procedure add_builtin(n : String; f : execute_function) is
      sym : symb_index;
   begin
      if get_symb(sym, n) then
         BBS.lisp.symbols.set_sym((kind => ST_DYNAMIC, d => sym), (Kind => SY_BUILTIN, f => f));
      else
         error("add_builtin", "Unable to add builtin symbol " & n);
      end if;
   end;
   --
   procedure add_special(n : String; f : special_function) is
      sym : symb_index;
   begin
      if get_symb(sym, n) then
         BBS.lisp.symbols.set_sym((kind => ST_DYNAMIC, d => sym), (Kind => SY_SPECIAL, s => f));
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
      sym : BBS.lisp.symbols.symbol;
      sym_flag : Boolean := False;
      e : element_type := NIL_ELEM;
      first : constant element_type := cons_table(s).car;
      rest : constant cons_index := BBS.lisp.evaluate.getList(cons_table(s).cdr);
      val : value;
   begin
      if first.kind = E_SYMBOL then
         sym := BBS.lisp.symbols.get_sym(first.sym);
         sym_flag := true;
      elsif first.kind = E_STACK then
         val := BBS.lisp.global.stack.search_frames(first.st_offset, first.st_name);
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
                  Print(sym.name);
                  New_Line;
               end if;
               sym.b.f.all(e, rest);
            when SY_SPECIAL =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating special ");
                  Print(sym.name);
                  New_Line;
               end if;
               sym.b.s.all(e, rest, PH_EXECUTE);
            when SY_LAMBDA =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating lambda ");
                  print(sym.b.ps);
                  new_line;
               end if;
               e := bbs.lisp.evaluate.func.eval_function(sym.b.ps, rest);
            when SY_VARIABLE =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating variable ");
                  print(sym.name);
                  new_line;
               end if;
               if (sym.b.pv.kind = E_VALUE) and then (sym.b.pv.v.kind = V_LAMBDA) then
                  if msg_flag then
                     Put("eval_dispatch: Evaluating lambda ");
                     print(sym.b.ps);
                     new_line;
                  end if;
                  e := bbs.lisp.evaluate.func.eval_function(sym.b.pv.v.lam, rest);
               else
                  BBS.lisp.memory.ref(sym.b.pv);
                  e := sym.b.pv;
               end if;
            when others =>
               if msg_flag then
                  Put("eval_dispatch: Evaluating unknown ");
                  print(sym.name);
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
         else
            BBS.lisp.memory.ref(s);
            e := BBS.lisp.evaluate.makeList(s);
         end if;
      elsif first.kind = E_STACK then
         val := BBS.lisp.global.stack.search_frames(first.st_offset, first.st_name);
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
