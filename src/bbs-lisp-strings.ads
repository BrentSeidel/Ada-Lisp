--
--  This package contains internal functions that support operations on the
--  lisp strings.
--
private package bbs.lisp.strings is
   --
   --  Converts a string to upper-case in place.
   --  In place lowercase is never used.  If it's ever needed, this routine
   --  can provide a template for it.
   --
   procedure uppercase(s : string_index)
     with Global => (In_Out => pvt_string_table);
   --
   --  Compare two strings
   --
   --   type comparison is (CMP_EQ, CMP_LT, CMP_GT, CMP_NE);
   --
   function compare(s1 : string_index; s2 : string_index) return comparison
     with Global => (Input => pvt_string_table);
   --
   --  Returns the length of a string in characters
   --
   function length(s : string_index) return int32
     with Global => (Input => pvt_string_table);
   --
   --  Converts a fixed length Ada string to a Lisp string.  Returns false if
   --  the Lisp string cannot be allocated.
   --
   function str_to_lisp(s : out string_index; str : String) return Boolean
     with Global => (Input => pvt_string_table);
   -- Should really be (In_Out => pvt_string_table);
   --
   --  Functions to append to an existing string.
   --
   function append(s : string_index; c : Character) return Boolean
     with Global => (Input => pvt_string_table);
   -- Should really be (In_Out => pvt_string_table);
   --
   --  Convert a character to upper-case
   --
   function To_Upper(c : Character) return Character
     with Global => Null;
   --
   --  Convert a character to lower-case
   --
   function To_Lower(c : Character) return Character
     with Global => Null;
end;
