--
--  This package contains internal functions that support operations on the
--  lisp strings.
--
package BBS.lisp.strings is
   --
   --  Types for string reference counts
   --
   type str_ref_count  is new Natural;
   FREE_STR  : constant str_ref_count := str_ref_count'First;
   --
   --  Converts a string to upper-case in place.
   --  In place lowercase is never used.  If it's ever needed, this routine
   --  can provide a template for it.
   --
   procedure uppercase(s : string_index);
   --
   --  type comparison is (CMP_EQ, CMP_LT, CMP_GT, CMP_NE);
   --
   --  Compare two Lisp strings
   --
   function compare(s1 : string_index; s2 : string_index) return comparison;
   --
   --   Compare a Lisp string with an Ada String
   --
   function compare(s1 : string_index; s2 : String) return comparison;
   --
   --  Returns the length of a string in characters
   --
   function length(s : string_index) return int32;
   --
   --  Converts a fixed length Ada string to a Lisp string.  Returns false if
   --  the Lisp string cannot be allocated.
   --
   function str_to_lisp(s : out string_index; str : String) return Boolean;
   -- Should really be (In_Out => pvt_string_table);
   --
   --  Functions to append to an existing string.  Returns False if an error
   --  occurs.
   --
   function append(s : string_index; c : Character) return Boolean;
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
   --  Copy helper function
   --
   type transform is (NONE, UPPER, LOWER);
   function copy(s : string_index; t : transform) return element_type;
   --
   --  Functions for character positions.
   --
   --  Given a string index and an offset, follow the link list to find the
   --  fragment that contains the offset and position in that fragment.  If the
   --  offset is beyond the end of the string,  str is set to NIL_STR and
   --  the offset to 0.
   --
   procedure cannonicalize(str : in out string_index; offset : in out Natural);
   --
   --  Update a string index and offset in cannonical form to point to the next
   --  character in cannonical form.  If not in cannonical form, or if the next
   --  character is past end end of the string, str is set to NIL_STR and offset
   --  is set to 0.
   --
   procedure move_to_next_char(str : in out string_index; offset : in out Natural);
   --
   --  Get a character at a cannonicalized position.  This must be a valid position.
   --
   function get_char_at(str : string_index; offset : Natural) return Character
       with pre => ((str /= NIL_STR) and (offset > 0));
   --
   --  Parse a string as an integer.  Starts at the first character in the string
   --  and proceeds until either the end of the string fragment or an illegal
   --  character is found.
   --
   function parse_integer(str : string_index) return int32
     with pre => (str /= NIL_STR);
   --
   --  Get a substring of a string.  An ending offset of -1 means the end of the
   --  source string.
   --
   function substring(str : string_index; start_offset : Integer; len : Integer)
                      return string_index
     with pre => (str /= NIL_STR);
   --
   --  Print a string
   --
   procedure print(s : string_index);
   --
   --  Dump the string table
   --
   procedure dump_strings;
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
   function is_end(self : str_iterator) return Boolean;
   --
   --  -------------------------------------------------------------------------
   --
   --  String memory management.
   --
   function count_free_str return Natural
     with Ghost;
   --
   --  Allocate a string
   --
   function alloc(s : out string_index) return Boolean
     with post => (if count_free_str = 0 then alloc'Result = False
                else alloc'Result = True);
   -- should really be (In_Out => pvt_string_table);
   --
   --  Increase the reference count of a string
   --
   procedure ref(s : string_index)
     with pre => (s > NIL_STR);
   --
   --  Decrease the reference count of a string and deallocate if the count
   --  reaches 0.
   --
   procedure deref(s : string_index)
     with pre => (s > NIL_STR);
   --
   --  Reset string table
   --
   procedure reset_string_table;
   --
   --  Get the reference count for a string fragment
   --
   function ref_count(s : string_index) return str_ref_count
     with pre => (s > NIL_STR);
   --
private
   --
   --  Structures and definitions for handling strings
   --
   fragment_len : constant Integer := 16;
   type fragment is
      record
         ref : str_ref_count;
         next : string_index;
         len : Integer range 0..fragment_len;
         str : String (1..fragment_len);
      end record;
   --
   --  The string table.
   --
   string_table : array (string_index'First + 1 .. string_index'Last) of fragment;

end;
