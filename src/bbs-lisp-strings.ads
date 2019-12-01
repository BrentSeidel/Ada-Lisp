--
--  This package contains internal functions that support operations on the
--  lisp strings.
--
private package bbs.lisp.strings is
   --
   --  Converts a string to upper-case in place.
   --
   procedure uppercase(s : string_index);
   --
   --  Converts a string to lower-case in place.
   --
   procedure lowercase(s : string_index);
   --
   --  Compare two strings
   --
   --   type comparison is (CMP_EQ, CMP_LT, CMP_GT, CMP_NE);
   function compare(s1 : string_index; s2 : string_index) return comparison;
   --
   --  Returns the length of a string in characters
   --
   function length(s : string_index) return Natural;
   --
   --  Converts a fixed length Ada string to a Lisp string.  Returns false if
   --  the Lisp string cannot be allocated.
   --
   function str_to_lisp(s : out string_index; str : String) return Boolean;
   --
   --  Functions to append to an existing string.
   --
   function append(s : string_index; c : Character) return Boolean;
end;
