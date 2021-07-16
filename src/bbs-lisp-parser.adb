with BBS.lisp;
with BBS.lisp.evaluate;
with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.symbols;
with BBS.lisp.utilities;
package body BBS.lisp.parser is
   --
   --  Append an element to a list.  Return true for success or false for failure.
   --  On failure, the list is dereffed.
   --
   function append_to_list(head : cons_index; e : element_type) return Boolean is
      current : cons_index;
   begin
      if elem_to_cons(current, e) then
         if not append(head, current) then
            error("append_to_list", "Unable to append to list");
            put("  Element: ");
            print(e, False, True);
            BBS.lisp.memory.deref(current);
            BBS.lisp.memory.deref(head);
            return False;
         end if;
      else
         error("append_to_list", "Unable to convert element to cons");
         put("  Element: ");
         print(e, False, True);
         BBS.lisp.memory.deref(current);
         BBS.lisp.memory.deref(head);
         return False;
      end if;
      return True;
   end;

   --
   --  This is the basic parser dispatcher.  Based on the first non-space character,
   --  parsing is dispatched to a lower level parser.
   --
   function parse(buff : parser_ptr; e : out element_type) return Boolean is
      head : cons_index;
      qtemp : constant Boolean := False;
   begin
      if list(buff, head, qtemp, True) then
         e := NIL_ELEM;
         if head /= NIL_CONS then
            e := cons_table(head).car;
            cons_table(head).car := NIL_ELEM;
            BBS.lisp.memory.deref(head);
         end if;
         return True;
      end if;
      error("parse", "Error in parsing list.");
      e := (kind => E_ERROR);
      return False;
   end;
   --
   --  Subfunction for parsing lists.  If the buffer ends before the end of the
   --  list is reached, more input is read and the parsing continues.
   --    buff   - Character source object
   --    s_expr - Parsed s expression
   --    qfixed - The list is quoted
   --
   function list(buff : parser_ptr; s_expr : out cons_index;
                 qfixed : Boolean; base : Boolean)
                 return Boolean is
      head : cons_index := NIL_CONS;
      temp : cons_index := NIL_CONS;
      str  : string_index;
      value : int32;
      e : element_type;
      list_end : Boolean := False;
      item : Natural := 0;
      special_flag : Boolean := False;
      special_symb : BBS.lisp.symbols.symbol := (ref => 1, str => NIL_STR, Kind => SY_EMPTY);
      begin_called : Boolean := False;
      item_count : Natural := 0;
      char : Character;
      test_char : Character;
      qtemp : Boolean := False;  --  Flag for quoting items
   begin
      s_expr := NIL_CONS;
      if not BBS.lisp.memory.alloc(head) then
         error("list", "Unable to allocate cons for head");
         return False;
      end if;
      while (not list_end) loop
         --
         --  Check for the end of the list
         --
         test_char := buff.get_char;
         if test_char = ')' then
            list_end := true;
            item := 0;
            if special_flag then
               if begin_called then
                  special_symb.s.all(e, head, PH_PARSE_END);
               else
                  error("list", "Internal error, parse end attempted to be called before parse begin");
                  put("Probably missing parameters to operation ");
                  print(special_symb.str);
                  new_line;
               end if;
            end if;
            qtemp := False;
         --
         --  Skip spaces
         --
         elsif isWhitespace(test_char) then
            buff.next_char;
         --
         -- Check for starting a new sub-list
         --
         elsif test_char = '(' then
            declare
               current : cons_index := NIL_CONS;
            begin
               buff.next_char;
               if list(buff, current, qfixed or qtemp, False) then
                  buff.next_char;
                  if (cons_table(current).car.kind = E_NIL) and (cons_table(current).cdr.kind = E_NIL) then
                     BBS.lisp.memory.deref(current);
                     if not append_to_list(head, NIL_ELEM) then
                        error("list", "Failure appending NIL_ELEM to list");
                        BBS.lisp.memory.deref(head);
                        return False;
                     end if;
                  else
                     if cons_table(head).car.kind = E_NIL then
                        cons_table(head).car := BBS.lisp.evaluate.makeList(current);
                     else
                        if BBS.lisp.memory.alloc(temp) then
                           cons_table(temp).car := BBS.lisp.evaluate.makeList(current);
                           if not append(head, temp) then
                              error("list", "Unable to append to list");
                              BBS.lisp.memory.deref(current);
                              BBS.lisp.memory.deref(head);
                              return False;
                           end if;
                        else
                           error("list", "Unable to convert element to cons");
                           BBS.lisp.memory.deref(current);
                           BBS.lisp.memory.deref(head);
                           return False;
                        end if;
                     end if;
                  end if;
               else
                  buff.next_char;
                  error ("list", "Error parsing list");
                  BBS.lisp.memory.deref(head);
                  return False;
               end if;
            end;
            qtemp := False;
         --
         --  Check for the start of an integer atom
         --
         elsif BBS.lisp.utilities.isDigit(test_char) or
           ((test_char = '-') and buff.is_next_digit) then
            int(buff, value);
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
            if cons_table(head).car.kind = E_NIL then
               cons_table(head).car := e;
            else
               if not append_to_list(head, e) then
                  error("list", "Failed appending decimal integer to list");
                  BBS.lisp.memory.deref(head);
                  return False;
               end if;
            end if;
            qtemp := False;
         --
         --  Check for special sequences.
         --
         elsif test_char = '#' then
            buff.next_char;
            if (buff.get_char = 'x') or (buff.get_char = 'X') then
               --
               --  Hexidecimal number
               --
               hex(buff, value);
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := e;
               else
                  if not append_to_list(head, e) then
                     error("list", "Failed appending hexidecimal integer to list");
                     BBS.lisp.memory.deref(head);
                     return False;
                  end if;
               end if;
            elsif buff.get_char = '\' then
               --
               --  Character literal
               --
               if parse_char(buff, char) then
                  e := (kind => E_VALUE, v => (kind => V_CHARACTER, c => char));
               else
                  e := (kind => E_ERROR);
               end if;
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := e;
               else
                  if not append_to_list(head, e) then
                     error("list", "Failed appending character to list");
                     BBS.lisp.memory.deref(head);
                     return False;
                  end if;
               end if;
            else
               error("list", "Unrecognized special form #" & buff.get_char);
               e := (Kind => E_ERROR);
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := e;
               else
                  if not append_to_list(head, e) then
                     error("list", "Failed appending error to list");
                     BBS.lisp.memory.deref(head);
                     return False;
                  end if;
               end if;
            end if;
            qtemp := False;
         --
         --  Check for the start of a string
         --
         elsif test_char = '"' then
            if parse_str(buff, str) then
               e := (kind => E_VALUE, v => (kind => V_STRING, s => str));
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := e;
               else
                  if not append_to_list(head, e) then
                     error("list", "Failed appending string to list");
                     return False;
                  end if;
               end if;
            else
               error("list", "Could not allocate string fragment.");
               BBS.lisp.memory.deref(head);
               return False;
            end if;
            qtemp := False;
         --
         --  Check for a comment
         --
         elsif test_char = ';' then
            buff.next_line;
            qtemp := False;
         --
         --  Quote the next element.
         --
         elsif test_char = ''' then
            buff.next_char;
            qtemp := True;
         --
         --  If nothing else, parse it as a symbol
         --
         else
            e := symb(buff, qfixed or qtemp);
            if cons_table(head).car.kind = E_NIL then
               cons_table(head).car := e;
            else
               if not append_to_list(head, e) then
                  error("list", "Failed appending symbol to list");
                  put("  Element: ");
                  print(e, False, True);
                  BBS.lisp.memory.deref(head);
                  return False;
               end if;
            end if;
            if (e.kind = E_SYMBOL) and not (qtemp or qfixed) then
               if (BBS.lisp.symbols.get_type(e.sym) = SY_SPECIAL) and (item = 0) then
                  special_flag := True;
                  special_symb := BBS.lisp.symbols.get_sym(e.sym);
                  special_symb.s.all(e, head, PH_QUERY);
                  if e.kind = E_VALUE then
                     if e.v.kind = V_INTEGER then
                        if e.v.i >= 0 then
                           item_count := Natural(e.v.i);
                        else
                           error("list", "Query returned value less than 0");
                           BBS.lisp.memory.deref(head);
                           return False;
                        end if;
                     else
                        error("list", "Query did not return an integer");
                        BBS.lisp.memory.deref(head);
                        return False;
                     end if;
                  else
                     error("list", "Query did not return a value");
                     BBS.lisp.memory.deref(head);
                     return False;
                  end if;
               end if;
            end if;
            qtemp := False;
         end if;
         --
         --  If there is no text left to parse and it's not the end of a list,
         --  read some more text and point to the start of it.
         --
         if buff.is_end and base then
            list_end := True;
            exit;
         end if;
         if buff.is_end and (not list_end) then
            if not buff.request_more then
               BBS.lisp.memory.deref(head);
               return False;
               end if;
         end if;
         --
         --  For special functions, call the function after the first parameter
         --  has been processed.  This allows symbols to be created immediately.
         --  This may also be useful when local variables and parameters are on
         --  a stack.
         --
         if special_flag and then (item = item_count) then
            special_symb.s.all(e, head, PH_PARSE_BEGIN);
            begin_called := True;
         end if;
         item := item + 1;
      end loop;
      s_expr := head;
      return True;
   end;
   --
   --  Parse a symbol.  The boolean values "T" and "NIL" are also detected here.
   --
   function symb(buff : parser_ptr; quoted : Boolean)
                 return element_type is
      test : string_index;
      el : element_type;
      symb : symb_index;
      flag : Boolean;
      pragma Unreferenced(flag);
      --
      --  The append to string is assumed to succeed.  There is a slight chance
      --  that this won't succeed, but if so, there are probably bigger problems.
      --  This should be cleaned up at some point.
      --
   begin
      if BBS.lisp.strings.alloc(test) then
         while (buff.get_char /= ')') and (not isWhitespace(buff.get_char)) and buff.not_end loop
            flag := BBS.lisp.strings.append(test, buff.get_char);
            buff.next_char;
         end loop;
         BBS.lisp.strings.uppercase(test);
         --
         --  Check for boolean values (T or NIL).
         --
         if BBS.lisp.strings.compare(test, "T") = CMP_EQ then
            BBS.lisp.strings.deref(test);
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
         end if;
         if BBS.lisp.strings.compare(test, "NIL") = CMP_EQ then
            BBS.lisp.strings.deref(test);
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         end if;
         if BBS.lisp.strings.length(test) = 0 then
            BBS.lisp.strings.deref(test);
            return NIL_ELEM;
         end if;
         --
         -- Now check for symbols
         --
         if not quoted then
            el := find_variable(test, False);
         else
            if get_symb(symb, test) then
               el := (Kind => E_VALUE, v => (kind => V_QSYMBOL, qsym => symb));
            else
               el := (kind => E_ERROR);
               error("parse symbol", "Unable to allocate string fragment.");
            end if;
         end if;
         BBS.lisp.strings.deref(test);
         return el;
      else
         error("parse symbol", "Unable to allocate string fragment.");
      end if;
      return (kind => E_ERROR);
   end;
   --
   --  Parse an integer.
   --
   procedure int(buff : parser_ptr; value : out int32) is
      accumulate : int32 := 0;
      neg : Boolean := False;
   begin
      if buff.get_char = '-' then
         neg := true;
         buff.next_char;
      end if;
      while BBS.lisp.utilities.isDigit(buff.get_char) and buff.not_end loop
         accumulate := accumulate*10 + int32'Value(" " & buff.get_char);
         buff.next_char;
      end loop;
      if neg then
         value := -accumulate;
      else
         value := accumulate;
      end if;
   end;
   --
   --  Parse an integer in hexidecimal notation.
   --
   procedure hex(buff : parser_ptr; value : out int32) is
      accumulate : uint32 := 0;
   begin
      buff.next_char;
      while BBS.lisp.utilities.isHex(buff.get_char) and buff.not_end loop
         accumulate := accumulate*16 + BBS.lisp.utilities.hexDigit(buff.get_char);
         buff.next_char;
      end loop;
      value := uint32_to_int32(accumulate);
   end;
   --
   --  Parse strings.  Note that currently strings cannot be broken across lines.
   --  An end of line probably has the same effect as a closing quotation mark.
   --
   function parse_str(buff : parser_ptr; s : out string_index) return Boolean is
      str  : string_index;
   begin
      if BBS.lisp.strings.alloc(str) then
         s := str;
         buff.next_char;
         while (buff.get_char /= '"') and buff.not_end loop
            if not BBS.lisp.strings.append(str, buff.get_char) then
               bbs.lisp.strings.deref(str);
               return False;
            end if;
            buff.next_char;
         end loop;
      end if;
      buff.next_char;
      return True;
   end;
   --
   --  Parse characters.  Character literals are introduced by #\.  This function
   --  is called with the pointer pointing to the character immediately following
   --  the #\.  Some named characters are also recognized.
   --  Named characters recognized should include:
   --    Space
   --    Newline
   --    Tab
   --    Page
   --    Rubout
   --    Linefeed
   --    Return
   --    Backspace
   --
   function parse_char(buff : parser_ptr; c : out Character) return Boolean is
      temp : String(1 .. 10);
      index : Natural := 1;
   begin
      buff.next_char;
      c := buff.get_char;
      buff.next_char;
      if BBS.lisp.utilities.isAlpha(c) then
         temp(index) := BBS.lisp.strings.To_Upper(c);
         while BBS.lisp.utilities.isAlpha(buff.get_char) and buff.not_end and index < 10 loop
            index := index + 1;
            temp(index) := BBS.lisp.strings.To_Upper(buff.get_char);
            buff.next_char;
         end loop;
         if index = 1 then
            return True;
         elsif temp(1 .. index) = "SPACE" then
            c := ' ';
         elsif temp(1 .. index) = "NEWLINE" then
            c := Character'Val(10);
         elsif temp(1 .. index) = "TAB" then
            c := Character'Val(9);
         elsif temp(1 .. index) = "PAGE" then
            c := Character'Val(12);
         elsif temp(1 .. index) = "RUBOUT" then
            c := Character'Val(127);
         elsif temp(1 .. index) = "LINEFEED" then
            c := Character'Val(10);
         elsif temp(1 .. index) = "RETURN" then
            c := Character'Val(13);
         elsif temp(1 .. index) = "BACKSPACE" then
            c := Character'Val(8);
         else
            error("parse_char", "Unrecognized character name <#\" &
                    temp(1 .. index) & ">");
            c := Character'Val(0);
            return False;
         end if;
      end if;
      return True;
   end;
   --
end;
