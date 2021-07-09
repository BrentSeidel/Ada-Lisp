--
--  This package contains internal functions that support operations on the
--  lisp strings.
--
private package bbs.lisp.strings is
   --
   --  Structures and definitions for handling strings
   --
--   fragment_len : constant Integer := 16;
--   type fragment is
--      record
--         ref : str_ref_count;
--         next : string_index;
--         len : Integer range 0..fragment_len;
--         str : String (1..fragment_len);
--      end record;
   --
--   string_table : array (string_index'First + 1 .. string_index'Last) of fragment
--     with Part_Of => pvt_string_table;
   --
   --  Converts a string to upper-case in place.
   --  In place lowercase is never used.  If it's ever needed, this routine
   --  can provide a template for it.
   --
   procedure uppercase(s : string_index)
     with Global => (In_Out => pvt_string_table);
   --
   --  type comparison is (CMP_EQ, CMP_LT, CMP_GT, CMP_NE);
   --
   --  Compare two Lisp strings
   --
   function compare(s1 : string_index; s2 : string_index) return comparison
     with Global => (Input => pvt_string_table);
   --
   --   Compare a Lisp string with an Ada String
   --
   function compare(s1 : string_index; s2 : String) return comparison;
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
   --  Functions to append to an existing string.  Returns False if an error
   --  occurs.
   --
   function append(s : string_index; c : Character) return Boolean
     with Global => (Input => pvt_string_table);
   -- Should really be (In_Out => pvt_string_table);
   --
   --  Appends the string pointed to by str to the string pointed to by dest.
   --  Note that dest is updated to point to the last fragment in the string.
   --  This can be used to efficiently append multiple strings.  However, the
   --  pointer to the head of the destination string will need to be saved
   --  elsewhere.  Returns False if an error occurs.
   --
   function append(dest : in out string_index; str : string_index) return Boolean
   with pre => (dest /= NIL_STR);
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
   --
   --  -------------------------------------------------------------------------
   --
   --  String iterator.  This can be used for looping through the characters in
   --  a string.  This is designed primarily to be used by BBS.lisp.parser.string.
   --
   type str_iterator is record
      base    : string_index;
      current : string_index;
      ptr     : Natural;
   end record;
   --
   --  Initializes the object to contain valid values
   --
   procedure init(self : in out str_iterator; s : string_index);
   --
   --  Gets the character selected by ptr.
   --
   function get_char(self : str_iterator) return Character;
   --
   --  Looks ahead to the next character after the current one
   --
   function get_next_char(self : str_iterator) return Character;
   --
   --  Increment ptr to point to the next character
   --
   procedure next_char(self : in out str_iterator);
   --
   --  Tests if the end of the string has been reached.
   --
   function is_end(self : str_iterator) return Boolean is ((self.current = NIL_STR) or else
                                                             (self.ptr > string_table(self.current).len));
end;
