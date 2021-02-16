--
--  This package contains operations for the character data type.
--
package BBS.lisp.evaluate.char is
   --
   --  Given a character, return the integer code for the character.  Typically
   --  the ASCII value.
   --
   procedure char_code(e : out element_type; s : cons_index);
   --
   --  Given an integer, return the character with that code or an error.
   --
   procedure code_char(e : out element_type; s : cons_index);
   --
   --  If character is alphabetic, convert to upper case.
   --
   procedure char_upcase(e : out element_type; s : cons_index);
   --
   --  If character is alphabetic, convert to lower case.
   --
   procedure char_downcase(e : out element_type; s : cons_index);
end;
