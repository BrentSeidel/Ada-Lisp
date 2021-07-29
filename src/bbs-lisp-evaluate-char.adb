with BBS.lisp.strings;
package body BBS.lisp.evaluate.char is
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   procedure char_code(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if t = NIL_CONS then
         error("char_int", "Internal error.  Should have a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("char_int", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_CHARACTER then
         e := (kind => V_INTEGER, i => Character'Pos(p1.c));
      else
         error("char_int", "Parameter must be of character type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   --  Given an integer, return the character with that code or an error.
   --
   procedure code_char(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if t = NIL_CONS then
         error("int_char", "Internal error.  Should have a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("int_char", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_INTEGER then
         if (p1.i >= 0) and (p1.i <= 255) then
            e := (kind => V_CHARACTER, c => Character'Val(p1.i));
         else
            error("int_char", "Parameter must be in range 0-255.  Value was "
                  & Integer'Image(Integer(p1.i)));
            e := make_error(ERR_UNKNOWN);
         end if;
      else
         error("int_char", "Parameter must be of integer type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   --  If character is alphabetic, convert to upper case.
   --
   procedure char_upcase(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if t = NIL_CONS then
         error("char_upcase", "Internal error.  Should have a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("char_upcase", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_CHARACTER then
         e := (kind => V_CHARACTER, c => BBS.lisp.strings.To_Upper(p1.c));
      else
         error("char_upcase", "Parameter must be of character type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   --  If character is alphabetic, convert to lower case.
   --
   procedure char_downcase(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if t = NIL_CONS then
         error("char_upcase", "Internal error.  Should have a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("char_upcase", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_CHARACTER then
         e := (kind => V_CHARACTER, c => BBS.lisp.strings.To_Lower(p1.c));
      else
         error("char_upcase", "Parameter must be of character type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
end;
