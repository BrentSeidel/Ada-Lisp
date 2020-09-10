--
--  This package contains functions relating to processing strings.
--
package BBS.lisp.evaluate.str is
   --
   --  Return the length of a string or list.  Atoms will get a value of 1.
   --  A nil pointer returns a length of 0.
   --
   function length(e : element_type) return element_type;
   --
   --  Return a specified character from a string.
   --
   function char(e : element_type) return element_type;
   --
   --  Parse a string as an integer and return the integer value.
   --
   function parse_integer(e : element_type) return element_type;
   --
   --  Return a substring of the original string
   --
   function subseq(e : element_type) return element_type;
   --
   --  Convert a string to upper case
   --
   function string_upcase(e : element_type) return element_type;
   --
   --  Convert a string to lower case
   --
   function string_downcase(e : element_type) return element_type;
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
