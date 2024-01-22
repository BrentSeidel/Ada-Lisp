with BBS.lisp.conses;
with BBS.lisp.evaluate;
with BBS.lisp.strings;
with BBS.lisp.symbols;
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
            BBS.lisp.conses.deref(current);
            BBS.lisp.conses.deref(head);
            return False;
         end if;
      else
         error("append_to_list", "Unable to convert element to cons");
         put("  Element: ");
         print(e, False, True);
         BBS.lisp.conses.deref(current);
         BBS.lisp.conses.deref(head);
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
            e := BBS.lisp.conses.get_car(head);
            BBS.lisp.conses.set_car(head, NIL_ELEM);
            BBS.lisp.conses.deref(head);
         end if;
         return True;
      end if;
      error("parse", "Error in parsing list.");
      e := make_error(ERR_PARSECHAR);
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
      special_symb : BBS.lisp.symbols.sym_body := (kind => SY_EMPTY);
      special_ptr  : symbol_ptr := (kind => ST_NULL);
      begin_called : Boolean := False;
      item_count : Natural := 0;
      char : Character;
      test_char : Character;
      qtemp : Boolean := False;  --  Flag for quoting items
   begin
      s_expr := NIL_CONS;
      if not BBS.lisp.conses.alloc(head) then
         error("parse list", "Unable to allocate cons for head");
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
                  error("parse list", "Internal error, parse end attempted to be called before parse begin");
                  put("Probably missing parameters to operation ");
                  if special_ptr.kind = ST_FIXED then
                     put(BBS.lisp.symbols.get_name(special_ptr).all);
                  else
                     print(BBS.lisp.symbols.get_name(special_ptr));
                  end if;
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
                  if (BBS.lisp.conses.get_car(current) = NIL_ELEM) and (BBS.lisp.conses.get_cdr(current) = NIL_ELEM) then
                     BBS.lisp.conses.deref(current);
                     if not append_to_list(head, NIL_ELEM) then
                        error("parse list", "Failure appending NIL_ELEM to list");
                        BBS.lisp.conses.deref(head);
                        return False;
                     end if;
                  else
                     if BBS.lisp.conses.get_car(head) = NIL_ELEM then
                        BBS.lisp.conses.set_car(head, BBS.lisp.evaluate.makeList(current));
                     else
                        if BBS.lisp.conses.alloc(temp) then
                           BBS.lisp.conses.set_car(temp, BBS.lisp.evaluate.makeList(current));
                           if not append(head, temp) then
                              error("parse list", "Unable to append to list");
                              BBS.lisp.conses.deref(current);
                              BBS.lisp.conses.deref(head);
                              return False;
                           end if;
                        else
                           error("parse list", "Unable to convert element to cons");
                           BBS.lisp.conses.deref(current);
                           BBS.lisp.conses.deref(head);
                           return False;
                        end if;
                     end if;
                  end if;
               else
                  buff.next_char;
                  error ("parse list", "Error parsing list");
                  BBS.lisp.conses.deref(head);
                  return False;
               end if;
            end;
            qtemp := False;
         --
         --  Check for the start of an integer atom
         --
         elsif isDigit(test_char) or
           ((test_char = '-') and buff.is_next_digit) then
            int(buff, value);
            e := (kind => V_INTEGER, i => value);
            if BBS.lisp.conses.get_car(head) = NIL_ELEM then
               BBS.lisp.conses.set_car(head, e);
            else
               if not append_to_list(head, e) then
                  error("parse list", "Failed appending decimal integer to list");
                  BBS.lisp.conses.deref(head);
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
               e := (kind => V_INTEGER, i => value);
               if BBS.lisp.conses.get_car(head) = NIL_ELEM then
                  BBS.lisp.conses.set_car(head, e);
               else
                  if not append_to_list(head, e) then
                     error("parse list", "Failed appending hexidecimal integer to list");
                     BBS.lisp.conses.deref(head);
                     return False;
                  end if;
               end if;
            elsif buff.get_char = '\' then
               --
               --  Character literal
               --
               if parse_char(buff, char) then
                  e := (kind => V_CHARACTER, c => char);
               else
                  e := make_error(ERR_PARSECHAR);
               end if;
               if BBS.lisp.conses.get_car(head) = NIL_ELEM then
                  BBS.lisp.conses.set_car(head, e);
               else
                  if not append_to_list(head, e) then
                     error("parse list", "Failed appending character to list");
                     BBS.lisp.conses.deref(head);
                     return False;
                  end if;
               end if;
            else
               error("parse list", "Unrecognized special form #" & buff.get_char);
               e := make_error(ERR_PARSECHAR);
               if BBS.lisp.conses.get_car(head) = NIL_ELEM then
                  BBS.lisp.conses.set_car(head, e);
               else
                  if not append_to_list(head, e) then
                     error("parse list", "Failed appending error to list");
                     BBS.lisp.conses.deref(head);
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
               e := (kind => V_STRING, s => str);
               if BBS.lisp.conses.get_car(head) = NIL_ELEM then
                  BBS.lisp.conses.set_car(head, e);
               else
                  if not append_to_list(head, e) then
                     error("parse list", "Failed appending string to list");
                     return False;
                  end if;
               end if;
            else
               error("parse list", "Could not allocate string fragment.");
               BBS.lisp.conses.deref(head);
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
            if BBS.lisp.conses.get_car(head) = NIL_ELEM then
               BBS.lisp.conses.set_car(head, e);
            else
               if not append_to_list(head, e) then
                  error("parse list", "Failed appending symbol to list");
                  put("  Element: ");
                  print(e, False, True);
                  BBS.lisp.conses.deref(head);
                  return False;
               end if;
            end if;
            if (e.kind = V_SYMBOL) and not (qtemp or qfixed) then
               if (BBS.lisp.symbols.get_type(e.sym) = SY_SPECIAL) and (item = 0) then
                  special_flag := True;
                  special_ptr := e.sym;
                  special_symb := BBS.lisp.symbols.get_sym(special_ptr);
                  special_symb.s.all(e, head, PH_QUERY);
                  if e.kind = V_INTEGER then
                     if e.i >= 0 then
                        item_count := Natural(e.i);
                     else
                        error("parse list", "Query returned value less than 0");
                        BBS.lisp.conses.deref(head);
                        return False;
                     end if;
                  else
                     error("parse list", "Query did not return an integer");
                     BBS.lisp.conses.deref(head);
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
               BBS.lisp.conses.deref(head);
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
      symb : symbol_ptr;
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
         -- Now check for symbols
         --
         if not quoted then
            el := find_variable(test, False);
         else
            if get_symb(symb, test) then
               el := (kind => V_QSYMBOL, qsym => symb);
            else
               el := make_error(ERR_ALLOCSYM);
               error("parse symbol", "Unable to allocate symbol entry.");
            end if;
         end if;
         BBS.lisp.strings.deref(test);
         return el;
      else
         error("parse symbol", "Unable to allocate string fragment.");
      end if;
      return make_error(ERR_ALLOCSTR);
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
      while isDigit(buff.get_char) and buff.not_end loop
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
      while isHex(buff.get_char) and buff.not_end loop
         accumulate := accumulate*16 + hexDigit(buff.get_char);
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
      if isAlpha(c) then
         temp(index) := BBS.lisp.strings.To_Upper(c);
         while isAlpha(buff.get_char) and buff.not_end and index < 10 loop
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
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return uint32 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
end;
