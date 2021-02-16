--
--  This package contains functions relating to processing strings.
--
package BBS.lisp.evaluate.str is
   --
   --  Return the length of a string or list.  Atoms will get a value of 1.
   --  A NIL pointer returns a length of 0.
   --
   procedure length(e : out element_type; s : cons_index);
   --
   --  Return a specified character from a string.
   --
   procedure char(e : out element_type; s : cons_index);
   --
   --  Parse a string as an integer and return the integer value.
   --
   procedure parse_integer(e : out element_type; s : cons_index);
   --
   --  Return a substring of the original string
   --
   procedure subseq(e : out element_type; s : cons_index);
   --
   --  Convert a string to upper case
   --
   procedure string_upcase(e : out element_type; s : cons_index);
   --
   --  Convert a string to lower case
   --
   procedure string_downcase(e : out element_type; s : cons_index);
private
   --
   --  Helper functions for length
   --
   function length(s : cons_index) return int32;
   function length(s : string_index) return int32;
   --
   --  Copy string with transformations
   --
   type transform is (NONE, UPPER, LOWER);
   function copy(s : string_index; t : transform) return element_type;
end;
