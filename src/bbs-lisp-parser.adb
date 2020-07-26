with BBS.lisp;
with BBS.lisp.strings;
with BBS.lisp.memory;
package body bbs.lisp.parser is
   --
   --  Utilities to assist in parsing
   --
   function Is_Digit(c : Character) return Boolean is
   begin
      if (c >= '0' and c <= '9') then
         return True;
      else
         return False;
      end if;
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
--      is_list : Boolean := False;
--      is_atom : Boolean := False;
      value : Integer;
   begin
      e := (kind => E_NIL);
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
         end if;
      --
      --  Comment
      --
      elsif buff(ptr) = ';' then
         e := (Kind => E_NIL);
      --
      --  Integer
      --
      elsif Is_Digit(buff(ptr)) or
        ((buff(ptr) = '-') and Is_Digit(buff(ptr + 1))) then
         flag := int(ptr, buff, last, value);
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
      --
      -- String
      --
      elsif buff(ptr) = '"' then
         flag := parse_str(ptr, buff, last, str);
         if flag then
            e := (Kind => E_VALUE, v =>(kind => V_STRING, s => str));
         end if;
      --
      --  Symbol
      --
      else
         e := symb(ptr, buff, last);
         flag := true;
      end if;
      return flag;
   end;
   --
   --  Subfunction for parsing lists.  If the buffer ends before the end of the
   --  list is reached, more input is read and the parsing continues.
   --
   function list(ptr : in out integer; buff : in out String; last : in out Integer; s_expr : out cons_index)
                 return Boolean is
      head : cons_index;
      current : cons_index;
      temp : cons_index;
      str  : string_index;
      value : Integer;
      flag : Boolean;
      e : element_type;
      list_end : Boolean := False;
      item : Natural := 0;
      special_flag : Boolean := False;
      special_symb : symbol;
      begin_called : Boolean := False;
      item_count : Natural;
   begin
      flag := bbs.lisp.memory.alloc(head);
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
                  flag := elem_to_cons(current, NIL_ELEM);
                  flag := append(head, current);
               else
                  if cons_table(head).car.kind = E_NIL then
                     cons_table(head).car := (kind => E_CONS, ps => current);
                  else
                     flag := bbs.lisp.memory.alloc(temp);
                     if flag then
                        cons_table(temp).car := (Kind => E_CONS, ps => current);
                        flag := append(head, temp);
                     end if;
                  end if;
               end if;
            end if;

         --
         --  Check for the start of an integer atom
         --
         elsif Is_Digit(buff(ptr)) or
           ((buff(ptr) = '-') and Is_Digit(buff(ptr + 1))) then
            flag := int(ptr, buff, last, value);
            if flag then
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
               if cons_table(head).car.kind = E_NIL then
                  cons_table(head).car := (kind => E_VALUE, v => (kind => V_INTEGER, i => value));
               else
                  flag := elem_to_cons(current, e);
                  flag := append(head, current);
               end if;
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
                  flag := elem_to_cons(current, e);
                  flag := append(head, current);
               end if;
            else
               error("parse list", "Could not allocate string fragment.");
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
               flag := elem_to_cons(current, e);
               if flag then
                  flag := append(head, current);
                  if not flag then
                     error("parse list", "Could not append symbol to list.");
                  end if;
               else
                  error("parse list", "Could not allocate cons cell for symbol.");
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
                        end if;
                     else
                        error("list", "Query did not return an integer");
                     end if;
                  else
                     error("list", "Query did not return a value");
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
         if special_flag and item = item_count then
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
   --  Parse an integer
   --
   function int(ptr : in out integer; buff : String; last : Integer; value : out Integer)
                return Boolean is
      accumulate : Integer := 0;
      neg : Boolean := False;
   begin
      if buff(ptr) = '-' then
         neg := true;
         ptr := ptr + 1;
      end if;
      while Is_Digit(buff(ptr)) and (ptr <= Last) loop
         accumulate := accumulate*10 + Integer'Value(" " & buff(ptr));
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
   --  Parse strings.  Note that currently strings cannot be broken across lines.
   --  An end of line probably has the same effect as a closing quotation mark.
   --
   function parse_str(ptr : in out Integer; buff : in String;
                      last : in integer; s : out string_index) return Boolean is
      str  : string_index;
      next : string_index;
      first : string_index;
--      start : Integer := ptr;
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
   --  Procedure to skip white space
   --
   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer) is
   begin
      while (buff(ptr) = ' ') and (ptr < Last) loop
         ptr := ptr + 1;
      end loop;
   end;
end;
