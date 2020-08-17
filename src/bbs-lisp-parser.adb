with BBS.lisp;
with BBS.lisp.strings;
with BBS.lisp.memory;
package body bbs.lisp.parser is
   --
   --  Utilities to assist in parsing
   --
   --  Is character a decimal digit?
   --
   function isDigit(c : Character) return Boolean is
   begin
      return (c >= '0' and c <= '9');
   end;
   --
   --  Is character an alphabetic character
   function isAlpha(c : Character) return Boolean is
   begin
      return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
   end;
   --
   -- Is character a hexidecimal digit?
   --
   function isHex(c : Character) return Boolean is
   begin
      return (c >= '0' and c <= '9') or (c >= 'A' and c <= 'F')
        or (c >= 'a' and c <= 'f');
   end;
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
   --  Procedure to skip white space
   --
   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer) is
   begin
      while (buff(ptr) = ' ') and (ptr < Last) loop
         ptr := ptr + 1;
      end loop;
   end;
   --
   --  Append an element to a list.  Return true for success or false for failure.
   --  On failure, the list is dereffed.
   --
   function append_to_list(head : cons_index; e : element_type) return Boolean is
      current : cons_index;
      flag : Boolean;
   begin
      flag := elem_to_cons(current, e);
      if flag then
         flag := append(head, current);
         if not flag then
            error("append_to_list", "Unable to append to list");
            BBS.lisp.memory.deref(current);
            BBS.lisp.memory.deref(head);
            return False;
         end if;
      else
         error("append_to_list", "Unable to convert element to cons");
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
   function parse(buff : in out String; last : in out Integer; e : out element_type) return Boolean is
      ptr  : Integer;
      head : cons_index;
      str  : string_index;
      flag : Boolean := False;
      value : int32;
      char : Character;
   begin
      e := NIL_ELEM;
      ptr := buff'First;
      skip_whitespace(ptr, buff, last);
      --
      --  Start of a list
      --
      if buff(ptr) = '(' then
         flag := list(ptr, buff, last, head);
         if flag then
            if (cons_table(head).car.kind = E_NIL) and (cons_table(head).cdr.kind = E_NIL) then
               e := NIL_ELEM;
               BBS.lisp.memory.deref(head);
            else
               e := (kind => E_CONS, ps => head);
            end if;
         else
            error("parse", "Error in parsing list.");
            BBS.lisp.memory.deref(head);
            e := (kind => E_ERROR);
         end if;
      --
      --  Comment
      --
      elsif buff(ptr) = ';' then
         e := (Kind => E_NIL);
      --
      --  Integer
      --
      elsif isDigit(buff(ptr)) or
        ((buff(ptr) = '-') and isDigit(buff(ptr + 1))) then
         flag := int(ptr, buff, last, value);
         if flag then
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
         else
            error("parse", "Error parsing number");
            e := (kind => E_ERROR);
         end if;
      --
      -- String
      --
      elsif buff(ptr) = '"' then
         flag := parse_str(ptr, buff, last, str);
         if flag then
            e := (Kind => E_VALUE, v =>(kind => V_STRING, s => str));
         else
            error("parse", "Error parsing string");
            BBS.lisp.memory.deref(str);
            e := (kind => E_ERROR);
         end if;
      --
      -- Special
      --
      elsif buff(ptr) = '#' then
         ptr := ptr + 1;
         if (buff(ptr) = 'x') or (buff(ptr) = 'X') then
            flag := hex(ptr, buff, last, value);
            if flag then
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
            else
               error("parse", "Error parsing hexidecimal integer");
               e := (kind => E_ERROR);
            end if;
         elsif buff(ptr) = '\' then
            --
            --  Character literal
            --
            flag := parse_char(ptr, buff, last, char);
            if flag then
               e := (kind => E_VALUE, v => (kind => V_CHARACTER, c => char));
            else
               e := (kind => E_ERROR);
               error("list", "Unable to parse character literal");
            end if;
         else
            error("parse", "Unrecognized special form");
            e := (kind => E_ERROR);
         end if;
      --
      --  Anything that doesn't match something else is treated as a symbol
      --
      else
         e := symb(ptr, buff, last);
         if e.kind /= E_ERROR then
            flag := true;
         else
            error("parse", "Error parsing symbol");
            flag := False;
         end if;
      end if;
      return flag;
   end;
   --
   --  Subfunction for parsing lists.  If the buffer ends before the end of the
   --  list is reached, more input is read and the parsing continues.
   --
   function list(ptr : in out integer; buff : in out String; last : in out Integer; s_expr : out cons_index)
                 return Boolean is
      head : cons_index := -1;
      current : cons_index := -1;
      temp : cons_index := -1;
      str  : string_index;
      value : int32;
      flag : Boolean;
      e : element_type;
      list_end : Boolean := False;
      item : Natural := 0;
      special_flag : Boolean := False;
      special_symb : symbol;
      begin_called : Boolean := False;
      item_count : Natural := 0;
      char : Character;
   begin
      flag := bbs.lisp.memory.alloc(head);
      if not flag then
         error("list", "Unable to allocate cons for head");
         return False;
      end if;
      ptr := ptr + 1;
      while (not list_end) loop
         skip_whitespace(ptr, buff, last);
         --
         --  Check for the end of the list
         --
         if buff(ptr) = ')' then
            list_end := true;
            if special_flag then
               if begin_called then
                  e := special_symb.s.all((kind => E_CONS, ps => head), PH_PARSE_END);
               else
                  error("list", "Internal error, parse end attempted to be called before parse begin");
               end if;
            end if;
         --
         -- Check for starting a new sub-list
         --
         elsif buff(ptr) = '(' then
            flag := list(ptr, buff, last, current);
            ptr := ptr + 1;
            if flag then
               if (cons_table(current).car.kind = E_NIL) and (cons_table(current).cdr.kind = E_NIL) then
                  BBS.lisp.memory.deref(current);
                  flag := append_to_list(head, NIL_ELEM);
                  if not flag then
                     error("list", "Failure appending NIL_ELEM to list");
                     return flag;
                  end if;
               else
                  if cons_table(head).car.kind = E_NIL then
                     cons_table(head).car := (kind => E_CONS, ps => current);
                  else
                     flag := BBS.lisp.memory.alloc(temp);
                     if flag then
                        cons_table(temp).car := (Kind => E_CONS, ps => current);
                        flag := append(head, temp);
                        if not flag then
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
               error ("list", "Error parsing list");
               BBS.lisp.memory.deref(current);
               BBS.lisp.memory.deref(head);
               return False;
            end if;
         --
         --  Check for the start of an integer atom
         --
         elsif isDigit(buff(ptr)) or
           ((buff(ptr) = '-') and isDigit(buff(ptr + 1))) then
            flag := int(ptr, buff, last, value);
            if flag then
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := e;
               else
                  flag := append_to_list(head, e);
                  if not flag then
                     error("list", "Failed appending decimal integer to list");
                     return flag;
                  end if;
               end if;
            else
               error ("list", "Error parsing integer");
               BBS.lisp.memory.deref(current);
               BBS.lisp.memory.deref(head);
               return False;
            end if;
         --
         --  Check for special sequences.
         --
         elsif buff(ptr) = '#' then
            ptr := ptr + 1;
            if (buff(ptr) = 'x') or (buff(ptr) = 'X') then
               --
               --  Hexidecimal number
               --
               flag := hex(ptr, buff, last, value);
               if flag then
                  e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
                  if cons_table(head).car.kind = E_NIL then
                     cons_table(head).car := e;
                  else
                  flag := append_to_list(head, e);
                     if not flag then
                        error("list", "Failed appending hexidecimal integer to list");
                        return flag;
                     end if;
                  end if;
               else
                  error("list", "Error parsing hexidecimal number");
                  BBS.lisp.memory.deref(current);
                  BBS.lisp.memory.deref(head);
                  return False;
               end if;
            elsif buff(ptr) = '\' then
               --
               --  Character literal
               --
               flag := parse_char(ptr, buff, last, char);
               if flag then
                  e := (kind => E_VALUE, v => (kind => V_CHARACTER, c => char));
                  if cons_table(head).car.kind = E_NIL then
                     cons_table(head).car := e;
                  else
                     flag := append_to_list(head, e);
                     if not flag then
                        error("list", "Failed appending character to list");
                        return flag;
                     end if;
                  end if;
               else
                  error("list", "Unable to parse character literal");
                  BBS.lisp.memory.deref(current);
                  BBS.lisp.memory.deref(head);
                  return False;
               end if;
            else
               error("list", "Unrecognized special form");
               BBS.lisp.memory.deref(current);
               BBS.lisp.memory.deref(head);
               return False;
            end if;
         --
         --  Check for the start of a string
         --
         elsif buff(ptr) = '"' then
            flag := parse_str(ptr, buff, last, str);
            if flag then
               e := (kind => E_VALUE, v => (kind => V_STRING, s => str));
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := (kind => E_VALUE, v => (kind => V_STRING, s => str));
               else
                  flag := append_to_list(head, e);
                  if not flag then
                     error("list", "Failed appending string to list");
                     return flag;
                  end if;
               end if;
            else
               error("list", "Could not allocate string fragment.");
               BBS.lisp.memory.deref(current);
               BBS.lisp.memory.deref(head);
               return False;
            end if;
         --
         --  Check for a comment
         --
         elsif buff(ptr) = ';' then
            ptr := last + 1;
         --
         --  If nothing else, parse it as a symbol
         --
         else
            e := symb(ptr, buff, last);
            if cons_table(head).car.kind = E_NIL then
               cons_table(head).car := e;
            else
               flag := append_to_list(head, e);
               if not flag then
                  error("list", "Failed appending symbol to list");
                  return flag;
               end if;
            end if;
            if e.kind = E_SYMBOL then
               if (symb_table(e.sym).kind = SY_SPECIAL) and (item = 0) then
                  special_flag := True;
                  special_symb := symb_table(e.sym);
                  e := special_symb.s.all((kind => E_CONS, ps => head), PH_QUERY);
                  if e.kind = E_VALUE then
                     if e.v.kind = V_INTEGER then
                        if e.v.i >= 0 then
                           item_count := Natural(e.v.i);
                        else
                           error("list", "Query returned value less than 0");
                           BBS.lisp.memory.deref(current);
                           BBS.lisp.memory.deref(head);
                           return False;
                        end if;
                     else
                        error("list", "Query did not return an integer");
                        BBS.lisp.memory.deref(current);
                        BBS.lisp.memory.deref(head);
                        return False;
                     end if;
                  else
                     error("list", "Query did not return a value");
                     BBS.lisp.memory.deref(current);
                     BBS.lisp.memory.deref(head);
                     return False;
                  end if;
               end if;
            end if;
         end if;
         --
         --  If there is no text left to parse and it's not the end of a list,
         --  read some more text and point to the start of it.
         --
         if (ptr > last) and (not list_end) then
            Put("More> ");
            Get_Line(buff, last);
            ptr := 1;
         end if;
         --
         --  For special functions, call the function after the first parameter
         --  has been processed.  This allows symbols to be created immediately.
         --  This may also be useful when local variables and parameters are on
         --  a stack.
         --
         if special_flag and then (item = item_count) then
            e := special_symb.s.all((kind => E_CONS, ps => head), PH_PARSE_BEGIN);
            begin_called := True;
         end if;
         item := item + 1;
      end loop;
      s_expr := head;
      return flag;
   end;
   --
   --  Parse a symbol.  The boolean values "T" and "NIL" are also detected here.
   --
   function symb(ptr : in out integer; buff : String; last : Integer)
                 return element_type is
      test : string_index;
      el : element_type;
      flag : Boolean;
   begin
      flag := BBS.lisp.memory.alloc(test);
      if flag then
         while (buff(ptr) /= ')') and (buff(ptr) /= ' ') and (ptr <= Last) loop
            flag := BBS.lisp.strings.append(test, buff(ptr));
            ptr := ptr + 1;
         end loop;
         BBS.lisp.strings.uppercase(test);
         --
         --  Check for boolean values.
         --
         if (string_table(test).len = 1) and (string_table(test).str(1) = 'T') then
            BBS.lisp.memory.deref(test);
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
         end if;
         if (string_table(test).len = 3) and (string_table(test).str(1..3) = "NIL") then
            BBS.lisp.memory.deref(test);
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
         end if;
         --
         -- Now check for symbols
         --
         el := find_variable(test, False);
         BBS.lisp.memory.deref(test);
         return el;
      else
         error("parse symbol", "Unable to allocate string fragment.");
      end if;
      return (kind => E_NIL);
   end;
   --
   --  Parse an integer.
   --
   function int(ptr : in out integer; buff : String; last : Integer; value : out int32)
                return Boolean is
      accumulate : int32 := 0;
      neg : Boolean := False;
   begin
      if buff(ptr) = '-' then
         neg := true;
         ptr := ptr + 1;
      end if;
      while isDigit(buff(ptr)) and (ptr <= Last) loop
         accumulate := accumulate*10 + int32'Value(" " & buff(ptr));
         ptr := ptr + 1;
      end loop;
      if neg then
         value := -accumulate;
      else
         value := accumulate;
      end if;
      return True;
   end;
   --
   --  Parse an integer in hexidecimal notation.
   --
   function hex(ptr : in out integer; buff : String; last : Integer; value : out int32)
                return Boolean is
      accumulate : uint32 := 0;
   begin
      ptr := ptr + 1;
      while isHex(buff(ptr)) and (ptr <= Last) loop
         accumulate := accumulate*16 + hexDigit(buff(ptr));
         ptr := ptr + 1;
      end loop;
      value := uint32_to_int32(accumulate);
      return True;
   end;
   --
   --  Parse strings.  Note that currently strings cannot be broken across lines.
   --  An end of line probably has the same effect as a closing quotation mark.
   --
   function parse_str(ptr : in out Integer; buff : in String;
                      last : in integer; s : out string_index) return Boolean is
      str  : string_index;
      next : string_index;
      first : string_index;
      flag : Boolean;
   begin
      flag := bbs.lisp.memory.alloc(str);
      string_table(str).len := 0;
      string_table(str).next := -1;
      if flag then
         s := str;
         first := str;
         ptr := ptr + 1;
         while (buff(ptr) /= '"') and (ptr <= last) loop
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
                  return False;
               end if;
            end if;
            ptr := ptr + 1;
         end loop;
      end if;
      ptr := ptr + 1;
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
   function parse_char(ptr : in out Integer; buff : in String;
                       last : in Integer; c : out Character) return Boolean is
      temp : String(1 .. 10);
      index : Natural := 1;
   begin
      ptr := ptr + 1;
      c := buff(ptr);
      ptr := ptr + 1;
      if isAlpha(c) then
         temp(index) := BBS.lisp.strings.To_Upper(c);
         while isAlpha(buff(ptr)) and (ptr <= last) and index < 10 loop
            index := index + 1;
            temp(index) := BBS.lisp.strings.To_Upper(buff(ptr));
            ptr := ptr + 1;
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
            error("parse_char", "Unrecognized character name");
            c := Character'Val(0);
            return False;
         end if;
      end if;
      return True;
   end;
   --
end;
