--
--  This package contains operations for the character data type.
--
package BBS.lisp.evaluate.char is
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   function char_code(s : cons_index) return element_type;
   --
   --  Given an integer, return the character with that code or an error.
   --
   function code_char(s : cons_index) return element_type;
   --
   --  If character is alphabetic, convert to upper case.
   --
   function char_upcase(s : cons_index) return element_type;
   --
   --  If character is alphabetic, convert to lower case.
   --
   function char_downcase(s : cons_index) return element_type;
end;
