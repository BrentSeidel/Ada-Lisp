with BBS.lisp.memory;
with BBS.lisp.strings;
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
      BBS.lisp.strings.cannonicalize(str, index);
      if str = NIL_STR then
         error("char", "Index out of range");
         e := (kind => E_ERROR);
         return;
      end if;
      e := (kind => E_VALUE, v => (kind => V_CHARACTER,
                                   c => BBS.lisp.strings.get_char_at(str, index)));
   end;
   --
   --  Parse a string as an integer and return the integer value.
   --
   procedure parse_integer(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
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
      --  Now that we know we have a string, try and parse it.
      --
      e := (kind => E_VALUE, v => (kind => V_INTEGER, i => BBS.lisp.strings.parse_integer(p1.v.s)));
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
      stop : Integer;  -- Last character, -1 means end of source string
--      flag : Boolean;
      head : string_index;
--      new_frag : string_index;
--      temp : string_index;
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
         stop := stop - start + 1;  -- Convert last character position to length.
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
      BBS.lisp.strings.cannonicalize(source, start);
      if source = NIL_STR then
         error("subseq", "Index out of range");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now source and start are pointing at the first character.  Allocate
      --  The first fragment of the destination.
      --
      head := BBS.lisp.strings.substring(source, start, stop);
      BBS.lisp.memory.deref(p1);
      if head = NIL_STR then
         e := (kind => E_ERROR);
      else
         e := (kind => E_VALUE, v => (kind => V_STRING, s => head));
      end if;
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
      t1 := BBS.lisp.strings.copy(p1.v.s, BBS.lisp.strings.UPPER);
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
      t1 := BBS.lisp.strings.copy(p1.v.s, BBS.lisp.strings.LOWER);
      if t1 = NIL_ELEM then
         error("string_downcase", "Error occured copying string");
      end if;
      BBS.lisp.memory.deref(p1);
      e := t1;
   end;
   --
end;
