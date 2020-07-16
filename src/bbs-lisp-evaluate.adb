with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.utilities;
with BBS.lisp.stack;
package body BBS.lisp.evaluate is
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
      p1 : element_type;
      p2 : element_type;
      p3 : element_type;
      ret : element_type;
      flag : Boolean;
      stacked : Boolean := False;
      index : stack_index;

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
         msg("setq", "Called during parse phase.");
         if e.kind = E_CONS then
            p1 := cons_table(e.ps).car;  --  Should be symbol for setq
            p2 := cons_table(e.ps).cdr;
            p3 := cons_table(p2.ps).car; --  Should be a symbol or tempsym
            if p3.kind = E_SYMBOL then
               symb := p3.sym;
               if (symb_table(symb).kind = BUILTIN) or
                 (symb_table(symb).kind = SPECIAL) then
                  error("setq", "Can't assign a value to a builtin or special symbol");
                  return NIL_ELEM;
               end if;
            elsif p3.kind = E_TEMPSYM then
               flag := get_symb(symb, p3.tempsym);
               if flag then
                  BBS.lisp.memory.ref(p3.tempsym);
                  cons_table(p2.ps).car := (kind => E_SYMBOL, sym => symb);
               else
                  error("setq", "Unable to add symbol ");
               end if;
            else
               error("setq", "First parameter is not a symbol or temporary symbol.");
               put("Parameter type is " & ptr_type'Image(p3.kind));
            end if;
         else
            error("setq", "Something went horribly wrong and setq did not get a list");
         end if;
      elsif p = EXECUTE then
         msg("setq", "Called during execute phase.");
         if e.kind = E_CONS then
            p1 := cons_table(e.ps).car;  --  Should be symbol name
            p2 := cons_table(e.ps).cdr;  --  Should be value to be assigned
            if p1.kind = E_SYMBOL then
               symb := p1.sym;
            elsif (p1.kind = E_PARAM) or (p1.kind = E_LOCAL) then
               stacked := True;
            else
               error("setq", "First parameter is not a symbol.");
               Put_Line("Kind is " & ptr_type'Image(p1.kind));
               return NIL_ELEM;
            end if;
            if not stacked then
               if (symb_table(symb).kind = BUILTIN) or
                 (symb_table(symb).kind = SPECIAL) then
                  error("setq", "Can't assign a value to a builtin or special symbol");
                  return NIL_ELEM;
               end if;
            end if;
            --
            --  At this point, p1 should be an element containing a valid symbol and
            --  symb is the index to that symbol.
            --
            --
            --  Now determine what value to attach to the symbol.
            --
            if p2.kind = E_CONS then
               p3 := cons_table(p2.ps).car;
               if p3.kind = E_CONS then
                  ret := eval_dispatch(p3.ps);
               else -- p3 is an element
                  ret := BBS.lisp.utilities.indirect_elem(p3);
               end if;
               --
               --  Check for stack variables
               --
               if stacked then
                  if p1.kind = E_PARAM then
                     index := BBS.lisp.stack.search_frames(p1.p_offset, p1.p_name);
                     BBS.lisp.memory.deref((kind => E_VALUE, v => BBS.lisp.stack.stack(index).p_value));
                     BBS.lisp.memory.ref(ret);
                     if ret.kind = E_VALUE then
                        BBS.lisp.stack.stack(index).p_value := ret.v;
                     elsif ret.kind = E_CONS then
                        BBS.lisp.stack.stack(index).p_value := (kind => V_LIST, l => ret.ps);
                     end if;
                  elsif p1.kind = E_LOCAL then
                     index := BBS.lisp.stack.search_frames(p1.l_offset, p1.l_name);
                     BBS.lisp.memory.deref((kind => E_VALUE, v => BBS.lisp.stack.stack(index).l_value));
                     BBS.lisp.memory.ref(ret);
                     if ret.kind = E_VALUE then
                        BBS.lisp.stack.stack(index).l_value := ret.v;
                     elsif ret.kind = E_CONS then
                        BBS.lisp.stack.stack(index).l_value := (kind => V_LIST, l => ret.ps);
                     end if;
                  end if;
               else
                  deref_previous(symb);
                  BBS.lisp.memory.ref(ret);
                  symb_table(symb) := (ref => 1, Kind => VARIABLE,
                                       pv => ret, str => symb_table(symb).str);
               end if;
               return ret;
            elsif p2.kind = E_VALUE then -- Rare, CDR is an value.
               deref_previous(symb);
               --
               --  At some point, this needs to be updated to check for stack variables.
               --
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
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(e : element_type) return element_type is
   begin
      exit_flag := True;
      return NIL_ELEM;
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
                  string_table(str).next := next;
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
