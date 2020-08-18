--
--  This package contains operations for the character data type.
--
package BBS.lisp.evaluate.char is
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   function char_int(e : element_type) return element_type;
   --
   --  Given an integer, return the character with that code or an error.
   --
   function int_char(e : element_type) return element_type;
   --
   --  If character is alphabetic, convert to upper case.
   --
   function char_upcase(e : element_type) return element_type;
   --
   --  If character is alphabetic, convert to lower case.
   --
   function char_downcase(e : element_type) return element_type;
end;
