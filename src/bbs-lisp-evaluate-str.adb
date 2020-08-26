with BBS.lisp.utilities;
package body BBS.lisp.evaluate.str is
   --
   --  Return the length of a string or list.  Atoms will get a value of 1.
   --  A nil pointer returns a length of 0.
   --
   function length(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Parameter
   begin
      if e.kind /= E_CONS then
         error("length", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      first_value(e, p1, t);
      if isList(p1) then
         return (kind => E_VALUE, v => (kind => V_INTEGER,
                                       i => length(getList(p1))));
      elsif p1.kind = E_VALUE then
         if p1.v.kind = V_STRING then
            return (kind => E_VALUE, v => (kind => V_INTEGER,
                                          i => length(p1.v.s)));
         elsif p1.v.kind = V_NONE then
            return (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
         else
            return (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
         end if;
      elsif p1.kind = E_NIL then
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
      else
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
      end if;
   end;
   --
   --  Helper functions for length
   --
   function length(s : cons_index) return int32 is
      t : element_type := (kind => E_CONS, ps => s);
      c : int32 := 0;
   begin
      while isList(t) loop
         c := c + 1;
         t := cons_table(getList(t)).cdr;
      end loop;
      if t.kind /= E_NIL then
         c := c + 1;
      end if;
      return c;
   end;
   --
   function length(s : string_index) return int32 is
      t : string_index := s;
      c : int32 := 0;
   begin
      while t > string_index'First loop
         c := c + int32(string_table(t).len);
         t := string_table(t).next;
      end loop;
      return c;
   end;
   --
   --  Return a specified character from a string.
   --
   function char(e : element_type) return element_type is
      t : element_type;   --  Temporary variable
      p1 : element_type;  --  First parameter (string)
      p2 : element_type;  --  Second parameter (integer)
      str : string_index;
      index : Integer;
   begin
      if e.kind /= E_CONS then
         error("char", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      --
      --  Get first parameter.  It should be a string
      --
      first_value(e, p1, t);
      if p1.kind = E_VALUE then
         if p1.v.kind /= V_STRING then
            error("char", "First parameter should be a string, not "
                  & value_type'Image(p1.v.kind));
            return (kind => E_ERROR);
         end if;
      else
         error("char", "First parameter should be a string, not " &
                 ptr_type'Image(p1.kind));
         return (kind => E_ERROR);
      end if;
      first_value(t, p2, t);
      if p2.kind = E_VALUE then
         if p2.v.kind /= V_INTEGER then
            error("char", "Second parameter should be an integer, not "
                  & value_type'Image(p2.v.kind));
            return (kind => E_ERROR);
         end if;
      else
         error("char", "Second parameter should be an integer, not " &
                 ptr_type'Image(p2.kind));
         return (kind => E_ERROR);
      end if;
      str := p1.v.s;
      index := Integer(p2.v.i) + 1;
      if index < 0 then
         error("char", "Index out of range");
         return (kind => E_ERROR);
      end if;
      while (str > string_index'First) and then (index > string_table(str).len) loop
         index := index - string_table(str).len;
         str := string_table(str).next;
      end loop;
      if str > string_index'First then
         if index > string_table(str).len then
            error("char", "Index out of range");
            return (kind => E_ERROR);
         end if;
      else
         error("char", "Index out of range");
         return (kind => E_ERROR);
      end if;
      return (kind => E_VALUE, v => (kind => V_CHARACTER, c => string_table(str).str(index)));
   end;
   --
   --  Parse a string as an integer and return the integer value.
   --
   function parse_integer(e : element_type) return element_type is
      t : element_type;
      p1 : element_type; --  Parameter
      str : string_index;
      accumulate : int32 := 0;
      neg : Boolean := False;
      ptr : Integer;
   begin
      if e.kind /= E_CONS then
         error("length", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      first_value(e, p1, t);
      if p1.kind = E_VALUE then
         if p1.v.kind /= V_STRING then
            error("parse-integer", "Parameter must be a string, not " &
                    value_type'Image(p1.v.kind));
            return (kind => E_ERROR);
         end if;
      else
         error("parse-integer", "Parameter must be a string, not " &
                 ptr_type'Image(p1.kind));
         return (kind => E_ERROR);
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
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => accumulate));
      else
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
      end if;
   end;
   --
end;
