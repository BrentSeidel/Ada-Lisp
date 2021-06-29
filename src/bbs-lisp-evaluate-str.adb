with BBS.lisp.memory;
with BBS.lisp.strings;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate.str is
   --
   --  Return the length of a string or list.  Atoms will get a value of 1.
   --  A nil pointer returns a length of 0.
   --
   procedure length(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if s = NIL_CONS then
         error("length", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(t);
      if isList(p1) then
         e := (kind => E_VALUE, v => (kind => V_INTEGER,
                                       i => length(getList(p1))));
      elsif p1.kind = E_VALUE then
         if p1.v.kind = V_STRING then
            e := (kind => E_VALUE, v => (kind => V_INTEGER,
                                          i => BBS.lisp.strings.length(p1.v.s)));
         elsif p1.v.kind = V_NONE then
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
         else
            e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
         end if;
      elsif p1.kind = E_NIL then
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
      else
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
      end if;
   end;
   --
   --  Helper functions for length
   --
   function length(s : cons_index) return int32 is
      t : cons_index := s;
      last : cons_index;
      c : int32 := 0;
   begin
      while t > NIL_CONS loop
         c := c + 1;
         last := t;
         t := getList(cons_table(t).cdr);
      end loop;
      if cons_table(last).cdr.kind /= E_NIL then
         c := c + 1;
      end if;
      return c;
   end;
   --
   --
   --  Return a specified character from a string.
   --
   procedure char(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type;  --  First parameter (string)
      p2 : element_type;  --  Second parameter (integer)
      str : string_index;
      index : Integer;
   begin
      if s = NIL_CONS then
         error("char", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Get first parameter.  It should be a string
      --
      p1 := first_value(t);
      if p1.kind = E_VALUE then
         if p1.v.kind /= V_STRING then
            error("char", "First parameter should be a string, not "
                  & value_type'Image(p1.v.kind));
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("char", "First parameter should be a value, not " &
                 ptr_type'Image(p1.kind));
         e := (kind => E_ERROR);
         return;
      end if;
      p2 := first_value(t);
      if p2.kind = E_VALUE then
         if p2.v.kind /= V_INTEGER then
            error("char", "Second parameter should be an integer, not "
                  & value_type'Image(p2.v.kind));
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("char", "Second parameter should be an value, not " &
                 ptr_type'Image(p2.kind));
         e := (kind => E_ERROR);
         return;
      end if;
      str := p1.v.s;
      index := Integer(p2.v.i) + 1;
      if index < 1 then
         error("char", "Index out of range");
         e := (kind => E_ERROR);
         return;
      end if;
      while (str > string_index'First) and then (index > string_table(str).len) loop
         index := index - string_table(str).len;
         str := string_table(str).next;
      end loop;
      if str > string_index'First then
         if index > string_table(str).len then
            error("char", "Index out of range");
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("char", "Index out of range");
         e := (kind => E_ERROR);
         return;
      end if;
      e := (kind => E_VALUE, v => (kind => V_CHARACTER, c => string_table(str).str(index)));
   end;
   --
   --  Parse a string as an integer and return the integer value.
   --
   procedure parse_integer(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      str : string_index;
      accumulate : int32 := 0;
      neg : Boolean := False;
      ptr : Integer;
   begin
      if s = NIL_CONS then
         error("length", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = E_VALUE then
         if p1.v.kind /= V_STRING then
            error("parse-integer", "Parameter must be a string, not " &
                    value_type'Image(p1.v.kind));
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("parse-integer", "Parameter must be a value, not " &
                 ptr_type'Image(p1.kind));
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now that we know we have a string, try and parse it.  Make the
      --  assumption that the integer fits within the first fragment.
      --
      str := p1.v.s;
      ptr := 1;
      if string_table(str).len > 0 then
         if string_table(str).str(ptr) = '-' then
            neg := true;
            ptr := ptr + 1;
         end if;
         while BBS.lisp.utilities.isDigit(string_table(str).str(ptr))
           and (ptr <= string_table(str).len) loop
            accumulate := accumulate*10 + int32'Value(" " & string_table(str).str(ptr));
            ptr := ptr + 1;
         end loop;
         if neg then
            accumulate := -accumulate;
         end if;
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i => accumulate));
      else
         e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
      end if;
   end;
   --
   --  Return a substring of the original string
   --
   procedure subseq(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type;  --  Parameter 1 (string)
      source : string_index;  -- Source string
      p2 : element_type;  --  Parameter 2 (starting position)
      start : Integer;
      p3 : element_type;  --  Parameter 3 (ending position) (optional)
      stop : Integer;
      flag : Boolean;
      done : Boolean := False;
      head : string_index;
      new_frag : string_index;
      temp : string_index;
   begin
      if s = NIL_CONS then
         error("subseq", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  First parameter
      --
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("subseq", "Error reported evaluating first parameter.");
         e := p1;
         return;
      end if;
      if p1.kind /= E_VALUE then
         error("subseq", "First parameter does not evaluate to a value");
         e := (kind => E_ERROR);
         return;
      elsif p1.v.kind /= V_STRING then
         error("subseq", "First parameter is not a string");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      source := p1.v.s;
      --
      --  Second parameter
      --
      p2 := first_value(t);
      if p2.kind = E_ERROR then
         error("subseq", "Error reported evaluating second parameter.");
         BBS.lisp.memory.deref(p1);
         e := p2;
         return;
      end if;
      if p2.kind /= E_VALUE then
         error("subseq", "Second parameter does not evaluate to a value");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      elsif p2.v.kind /= V_INTEGER then
         error("subseq", "Second parameter is not an integer");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      start := Integer(p2.v.i) + 1;
      --
      --  Third parameter (optional)
      --
      if t > NIL_CONS then
         p3 := first_value(t);
         if p3.kind = E_ERROR then
            error("subseq", "Error reported evaluating third parameter.");
            BBS.lisp.memory.deref(p1);
            e := p3;
            return;
         end if;
         if p3.kind /= E_VALUE then
            error("subseq", "Third parameter does not evaluate to a value");
            BBS.lisp.memory.deref(p1);
            e := (kind => E_ERROR);
            return;
         elsif p3.v.kind /= V_INTEGER then
            error("subseq", "Third parameter is not an integer");
            BBS.lisp.memory.deref(p1);
            e := (kind => E_ERROR);
            return;
         end if;
         stop := Integer(p3.v.i);
         if stop < start then
            error("subseq", "Ending character must be greater than starting character.");
            BBS.lisp.memory.deref(p1);
            e := (kind => E_ERROR);
            return;
         end if;
         stop := stop - start + 1;
      else
         stop := -1;
      end if;
      if start < 1 then
         error("subseq", "Starting character must not be less than 0.");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now do the processing.  Find the starting character.
      --
      while (source > string_index'First) and then (start > string_table(source).len) loop
         start := start - string_table(source).len;
         source := string_table(source).next;
      end loop;
      if source > string_index'First then
         if start > string_table(source).len then
            error("subseq", "Index out of range");
            BBS.lisp.memory.deref(p1);
            e := (kind => E_ERROR);
            return;
         end if;
      else
         error("subseq", "Index out of range");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now source and start are pointing at the first character.  Allocate
      --  The first fragment of the destination.
      --
      flag := BBS.lisp.memory.alloc(head);
      if not flag then
         error("subseq", "Unable to allocate string fragment.");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      new_frag := head;
      for index in 1 .. fragment_len loop
         string_table(new_frag).str(index) := string_table(source).str(start);
         string_table(new_frag).len := string_table(new_frag).len + 1;
         if stop /= -1 then
            stop := stop - 1;
         end if;
         exit when stop = 0;
         start := start + 1;
         exit when (start > string_table(source).len) and (start <= fragment_len);
         if start > fragment_len then
            start := 1;
            source := string_table(source).next;
         end if;
         exit when source = string_index'First;
      end loop;
      if (start > string_table(source).len) and (start <= fragment_len) then
         BBS.lisp.memory.deref(p1);
         e := (kind => E_VALUE, v => (kind => V_STRING, s => head));
         return;
      end if;
      while (source /= string_index'First) and ((stop = -1) or (stop > 0)) loop
         flag := BBS.lisp.memory.alloc(temp);
         if not flag then
            error("subseq", "Unable to allocate string fragment.");
            BBS.lisp.memory.deref(head);
            BBS.lisp.memory.deref(p1);
            e := (kind => E_ERROR);
            return;
         end if;
         string_table(new_frag).next := temp;
         new_frag := temp;
         for index in 1 .. fragment_len loop
            string_table(new_frag).str(index) := string_table(source).str(start);
            string_table(new_frag).len := string_table(new_frag).len + 1;
            if stop /= -1 then
               stop := stop - 1;
            end if;
            exit when stop = 0;
            start := start + 1;
            if (start > string_table(source).len) and (start <= fragment_len) then
               done := True;
            end if;
            exit when done;
            if start > fragment_len then
               start := 1;
               source := string_table(source).next;
               if source = string_index'First then
                  done := True;
               end if;
            end if;
            exit when done;
         end loop;
         exit when done;
      end loop;
      BBS.lisp.memory.deref(p1);
      e := (kind => E_VALUE, v => (kind => V_STRING, s => head));
   end;
   --
   --  Copy helper function
   --
   function copy(s : string_index; t : transform) return element_type is
      flag : Boolean;
      source : string_index := s;
      new_frag : string_index;
      head : string_index;
      temp : string_index;
   begin
      flag := BBS.lisp.memory.alloc(head);
      if not flag then
         error("string copy", "Unable to allocate string fragment.");
         return (kind => E_ERROR);
      end if;
      new_frag := head;
      string_table(new_frag).len := string_table(source).len;
      for index in 1 .. fragment_len loop
         case t is
            when NONE =>
               string_table(new_frag).str(index) := string_table(source).str(index);
            when UPPER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Upper(string_table(source).str(index));
            when LOWER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Lower(string_table(source).str(index));
         end case;
      end loop;
      source := string_table(source).next;
      while source /= string_index'First loop
         flag := BBS.lisp.memory.alloc(temp);
         if not flag then
            error("string copy", "Unable to allocate string fragment.");
            BBS.lisp.memory.deref(head);
            return (kind => E_ERROR);
         end if;
         string_table(new_frag).next := temp;
         new_frag := temp;
         string_table(new_frag).len := string_table(source).len;
         for index in 1 .. fragment_len loop
            case t is
            when NONE =>
               string_table(new_frag).str(index) := string_table(source).str(index);
            when UPPER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Upper(string_table(source).str(index));
            when LOWER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Lower(string_table(source).str(index));
            end case;
         end loop;
         source := string_table(source).next;
      end loop;
      return (kind => E_VALUE, v => (kind => V_STRING, s => head));
   end;
   --
   --  Convert a string to upper case
   --
   procedure string_upcase(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      t1 : element_type;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if s = NIL_CONS then
         error("string_upcase", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("string_upcase", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("string_upcase", "Parameter does not evaluate to a value");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      if v.kind /= V_STRING then
         error("string_upcase", "Parameter must be of string type, not " & value_type'Image(v.kind));
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now that the parameter is determined to be of the correct type,
      --  copy it while converting to uppercase.
      --
      t1 := copy(p1.v.s, UPPER);
      if t1 = NIL_ELEM then
         error("string_upcase", "Error occured copying string");
      end if;
      BBS.lisp.memory.deref(p1);
      e := t1;
   end;
   --
   --  Convert a string to lower case
   --
   procedure string_downcase(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      t1 : element_type;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if s = NIL_CONS then
         error("string_downcase", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("string_downcase", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("string_downcase", "Parameter does not evaluate to a value");
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      if v.kind /= V_STRING then
         error("string_downcase", "Parameter must be of string type, not " & value_type'Image(v.kind));
         BBS.lisp.memory.deref(p1);
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now that the parameter is determined to be of the correct type,
      --  copy it while converting to lowercase.
      --
      t1 := copy(p1.v.s, LOWER);
      if t1 = NIL_ELEM then
         error("string_downcase", "Error occured copying string");
      end if;
      BBS.lisp.memory.deref(p1);
      e := t1;
   end;
   --
end;
