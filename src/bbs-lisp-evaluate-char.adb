with BBS.lisp.strings;
package body BBS.lisp.evaluate.char is
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   function char_int(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if t = NIL_CONS then
         error("char_int", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("char_int", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("char_int", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_CHARACTER then
         return (kind => E_VALUE, v => (kind => V_INTEGER, i => Character'Pos(v.c)));
      else
         error("char_int", "Parameter must be of character type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
   --  Given an integer, return the character with that code or an error.
   --
   function int_char(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if t = NIL_CONS then
         error("int_char", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("int_char", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("int_char", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_INTEGER then
         if (v.i >= 0) and (v.i <= 255) then
            return (kind => E_VALUE, v => (kind => V_CHARACTER, c => Character'Val(v.i)));
         else
            error("int_char", "Parameter must be in range 0-255.  Value was "
                  & Integer'Image(Integer(v.i)));
            return (Kind => E_ERROR);
         end if;
      else
         error("int_char", "Parameter must be of integer type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
   --  If character is alphabetic, convert to upper case.
   --
   function char_upcase(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if t = NIL_CONS then
         error("char_upcase", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("char_upcase", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("char_upcase", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_CHARACTER then
         return (kind => E_VALUE, v => (kind => V_CHARACTER,
                                        c => BBS.lisp.strings.To_Upper(v.c)));
      else
         error("char_upcase", "Parameter must be of character type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
   --  If character is alphabetic, convert to lower case.
   --
   function char_downcase(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if t = NIL_CONS then
         error("char_upcase", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("char_upcase", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("char_upcase", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_CHARACTER then
         return (kind => E_VALUE, v => (kind => V_CHARACTER,
                                        c => BBS.lisp.strings.To_Lower(v.c)));
      else
         error("char_upcase", "Parameter must be of character type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
end;
