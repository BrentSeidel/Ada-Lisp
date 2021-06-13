with BBS.lisp.utilities;
package body bbs.lisp.parser.string is
   --
   --  Initialize the buffer object
   --
   procedure init(self : in out parser_string; s : string_index) is
   begin
      self.ptr := 1;
      self.base := s;
      self.current := s;
   end;
   --
   --  Move pointer to point to the next character in the string
   --
   procedure next_char(self : in out parser_string) is
   begin
      if self.current > NIL_STR then
         self.ptr := self.ptr + 1;
         if (self.ptr > string_table(self.current).len) then
            self.ptr := 1;
            self.current := string_table(self.current).next;
         end if;
      end if;
   end;
   --
   --  This should advance the pointer until either an end of line character
   --  (ASCII 10 or 13) is reached or the end of the Lisp string is reached.
   --
   procedure next_line(self : in out parser_string) is
   begin
      self.current := NIL_STR;
   end;
   --
   --  Checks if the character after ptr is a digit.
   --
   function is_next_digit(self : parser_string) return Boolean is
      next : string_index;
   begin
      --
      --  Check easiest case first
      --
      if self.current = NIL_STR then
         return False;
      end if;
      --
      --  Now, we know that current points to an existing fragment.  Check if
      --  the next character is in the current fragment. (most common case)
      --
      if self.ptr < string_table(self.current).len then
         return BBS.lisp.utilities.isDigit(string_table(self.current).str(self.ptr));
      end if;
      --
      -- Check if the current fragment is the last.  If so, there is no next character.
      --
      next := string_table(self.current).next;
      if next = NIL_STR then
         return False;
      end if;
      --
      --  Make sure that the length of the next fragment is greater than 0
      --
      if string_table(next).len = 0 then
         return False;
      end if;
      --
      -- Check the first character in the next fragment
      --
      return BBS.lisp.utilities.isDigit(string_table(next).str(1));
   end;
   --
   function get_char(self : parser_string) return Character is
   begin
      if self.current > NIL_STR then
         if self.ptr <= string_table(self.current).len then
            return (string_table(self.current).str(self.ptr));
         end if;
      end if;
      return ' ';
   end;
end;
