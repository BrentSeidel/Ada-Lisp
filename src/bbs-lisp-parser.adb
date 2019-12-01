with Ada.Text_IO;
with Ada.Characters.Handling;
with BBS.lisp.strings;
package body bbs.lisp.parser is
   --
   --  Utilities to assist in parsing
   --
   --  This is the basic parser dispatcher.  Based on the first non-space character,
   --  parsing is dispatched to a lower level parser.
   --
   function parse(buff : in out String; last : in out Integer; e : out element_type) return Boolean is
      ptr  : Integer;
      head : cons_index;
      atom : atom_index;
      str  : string_index;
      flag : Boolean := False;
      is_list : Boolean := False;
      is_atom : Boolean := False;
      value : Integer;
   begin
      e := (kind => NIL_TYPE);
      ptr := buff'First;
      skip_whitespace(ptr, buff, last);
      --
      --  Start of a list
      --
      if buff(ptr) = '(' then
         flag := list(ptr, buff, last, head);
         if flag then
            e := (kind => CONS_TYPE, ps => head);
         end if;
      --
      --  Comment
      --
      elsif buff(ptr) = ';' then
         e := (Kind => NIL_TYPE);
      --
      --  Integer
      --
      elsif Ada.Characters.Handling.Is_Digit(buff(ptr)) or
        ((buff(ptr) = '-') and Ada.Characters.Handling.Is_Digit(buff(ptr + 1))) then
         flag := int(ptr, buff, last, value);
         flag := bbs.lisp.memory.alloc(atom);
         atom_table(atom) := (ref => 1, kind => ATOM_INTEGER, i => value);
         bbs.lisp.memory.lock(atom);
         e := (kind => ATOM_TYPE, pa => atom);
      --
      -- String
      --
      elsif buff(ptr) = '"' then
         flag := parse_str(ptr, buff, last, str);
         if flag then
            flag := bbs.lisp.memory.alloc(atom);
            if flag then
               atom_table(atom) := (ref => 1, kind => ATOM_STRING, str => str);
               e := (Kind => ATOM_TYPE, pa => atom);
            end if;
         end if;
      --
      --  Symbol
      --
      else
         flag := symb(ptr, buff, last, atom);
         if flag then
            e := (kind => ATOM_TYPE, pa => atom);
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
      head : cons_index;
      current : cons_index;
      temp : cons_index;
      atom : atom_index;
      str  : string_index;
      value : Integer;
      flag : Boolean;
      list_end : Boolean := False;
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
         --
         -- Check for starting a new sub-list
         --
         elsif buff(ptr) = '(' then
            flag := list(ptr, buff, last, current);
            flag := bbs.lisp.memory.alloc(temp);
            cons_table(temp).car := (Kind => CONS_TYPE, ps => current);
            ptr := ptr + 1;
            if flag then
               flag := append(head, temp);
            end if;
         --
         --  Check for the start of an integer atom
         --
         elsif Ada.Characters.Handling.Is_Digit(buff(ptr)) or
           ((buff(ptr) = '-') and Ada.Characters.Handling.Is_Digit(buff(ptr + 1))) then
            flag := int(ptr, buff, last, value);
            if flag then
               flag := bbs.lisp.memory.alloc(atom);
               if flag then
                  atom_table(atom) := (ref => 1, kind => ATOM_INTEGER, i => value);
                  if cons_table(head).car.kind = NIL_TYPE then
                     cons_table(head).car := (kind => ATOM_TYPE, pa => atom);
                  else
                     flag := atom_to_cons(current, atom);
                     flag := append(head, current);
                  end if;
               else
                  error("parse list", "Could not allocate atom for integer.");
                  return False;
               end if;
            end if;
         --
         --  Check for the start of a string
         --
         elsif buff(ptr) = '"' then
            flag := parse_str(ptr, buff, last, str);
            if flag then
               flag := bbs.lisp.memory.alloc(atom);
               if flag then
                  atom_table(atom) := (ref => 1, kind => ATOM_STRING, str => str);
                  if cons_table(head).car.kind = NIL_TYPE then
                     cons_table(head).car := (kind => ATOM_TYPE, pa => atom);
                  else
                     flag := atom_to_cons(current, atom);
                     flag := append(head, current);
                  end if;
               else
                  error("parse list", "Could not allocate atom for string.");
                  return False;
               end if;
            else
               error("parse list", "Could not allocate string fragment.");
               return False;
            end if;
         --
         --  Check for a comment
         --
         elsif buff(ptr) = ';' then
            ptr := last;
         --
         --  If nothing else, parse it as a symbol
         --
         else
            flag := symb(ptr, buff, last, atom);
            if flag then
               if cons_table(head).car.kind = NIL_TYPE then
                  cons_table(head).car := (kind => ATOM_TYPE, pa => atom);
               else
                  flag := atom_to_cons(current, atom);
                  flag := append(head, current);
               end if;
            end if;
         end if;
         --
         --  If there is no text left to parse and it's not the end of a list,
         --  read some more text and point to the start of it.
         --
         if (ptr > last) and (not list_end) then
            Ada.Text_IO.Put("More> ");
            Ada.Text_IO.Get_Line(buff, last);
            ptr := 1;
         end if;
      end loop;
      s_expr := head;
      return flag;
   end;
   --
   --  Parse a symbol
   --
   function symb(ptr : in out integer; buff : String; last : Integer; a : out atom_index)
                 return Boolean is
      test : string_index;
      symb : symb_index;
      tempsym : tempsym_index;
      flag : Boolean;
   begin
      flag := BBS.lisp.memory.alloc(test);
      if flag then
         while (buff(ptr) /= ')') and (buff(ptr) /= ' ') and (ptr <= Last) loop
            flag := BBS.lisp.strings.append(test, buff(ptr));
            ptr := ptr + 1;
         end loop;
         BBS.lisp.strings.uppercase(test);
         flag := find_symb(symb, test);
         if flag then
            flag := bbs.lisp.memory.alloc(a);
            BBS.lisp.memory.deref(test);
            if flag then
               atom_table(a) := (ref => 1, kind => ATOM_SYMBOL, sym => symb);
            end if;
         else
            flag := get_tempsym(tempsym, test);
            if flag then
               flag := bbs.lisp.memory.alloc(a);
               if flag then
                  atom_table(a) := (ref => 1, kind => ATOM_TEMPSYM, tempsym => tempsym);
               end if;
            end if;
         end if;
      else
         error("parse symbol", "Unable to allocate string fragment.");
      end if;
      return flag;
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
      while Ada.Characters.Handling.Is_Digit(buff(ptr)) and (ptr <= Last) loop
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
   function parse_str(ptr : in out Integer; buff : in out String;
                      last : in out integer; s : out string_index) return Boolean is
      str  : string_index;
      next : string_index;
      first : string_index;
      start : Integer := ptr;
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
                  string_table(str).next := Integer(next);
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
