--
--  This package contains data and routines for providing data to the parser
--  routines.  This is done to allow the parser to be decoupled from I/O.
--
package bbs.lisp.parser.string is
   --
   --  This object contains the following data:
   --    base    - Pointer to the first fragment of the string (may not be needed)
   --    current - Poiner to the current string fragment
   --    ptr     - Pointer to the current character in the string fragment.
   --
   type parser_string is new parser_buffer with
      record
         base    : string_index;
         current : string_index;
         ptr     : Natural;
      end record;
   type parser_string_ptr is access all parser_string'Class;
   --
   --  Gets the character selected by ptr.
   --
   overriding
   function get_char(self : parser_string) return Character;
   --
   --  Checks if the character after ptr is a digit.
   --
   overriding
   function is_next_digit(self : parser_string) return Boolean;
   --
   --  Increment ptr to point to the next character
   --
   overriding
   procedure next_char(self : in out parser_string);
   --
   --  Tests if ptr is less than or equal to last.
   --
   overriding
   function not_end(self : parser_string) return Boolean is ((self.current > NIL_STR) and then
                                                             (self.ptr <= string_table(self.current).len));
   --
   --  Tests if ptr is greater than last (the opposite of not_end)
   --
   overriding
   function is_end(self : parser_string) return Boolean is ((self.current = NIL_STR) or else
                                                             (self.ptr > string_table(self.current).len));
   --
   --  Sets ptr to be greater than last so that the tests not_end and is_end will
   --  indicate the end.
   --
   overriding
   procedure set_end(self : in out parser_string);
   --
   --  Used to request more data.  This will fail when reading a Lisp string.
   --
   overriding
   function request_more(self : in out parser_string) return Boolean is (False);
   --
   --  Initializes the object to contain valid values
   --
   procedure init(self : in out parser_string; s : string_index);

end;
