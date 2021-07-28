with BBS.lisp.utilities;
package body BBS.lisp.strings is
   --
   --  Procedure to print a string
   --
   procedure print(s : string_index) is
      next : string_index := s;
      nxt : string_index;
   begin
      while next > NIL_STR loop
         nxt := next;
         Put(string_table(nxt).str(1..string_table(nxt).len));
         next := string_table(nxt).next;
      end loop;
   end;
   --
   --  For debugging, dump all strings
   --
   procedure dump_strings is
   begin
      for i in string_index'First + 1 .. string_index'Last loop
         if string_table(i).ref > 0 then
            Put("String " & Integer'Image(Integer(i)) & " contains: <"
                                 & string_table(i).str & ">, ");
            Put("Reference count: " & str_ref_count'Image(string_table(i).ref));
            Put(", Length: " & Integer'Image(Integer(string_table(i).len)));
            Put_Line(", Next: " & string_index'Image(string_table(i).next));
         end if;
      end loop;
   end;
   --
   --  Converts a string to upper-case in place.
   --
   procedure uppercase(s : string_index) is
      next : string_index := s;
      nxt : string_index;
   begin
      while next > NIL_STR loop
         nxt := next;
         for i in 1 .. string_table(nxt).len loop
            string_table(nxt).str(i) := To_Upper(string_table(nxt).str(i));
         end loop;
         next := string_table(nxt).next;
      end loop;
   end;
   --
   --  Compare two Lisp strings.  This loops through the two strings and compares
   --  character by character.  As soon as a character is not equal, it returns
   --  CMP_LT or CMP_GT.  If one string is longer than the the other, but
   --  otherwise equal, the longer string is greater than the shorter one.
   --
   function compare(s1 : string_index; s2 : string_index) return comparison is
      next1 : string_index := s1;
      next2 : string_index := s2;
      nxt1 : string_index;
      nxt2 : string_index;
      limit : Integer;
   begin
      while (next1 > NIL_STR) and (next2 > NIL_STR) loop
         limit := string_table(next1).len;
         if string_table(next2).len < limit then
            limit := string_table(next2).len;
         end if;
         for i in 1 .. limit loop
            if string_table(next1).str(i) < string_table(next2).str(i) then
               return CMP_LT;
            elsif string_table(next1).str(i) > string_table(next2).str(i) then
               return CMP_GT;
            end if;
         end loop;
         --
         --  Save current fragment indices so that they will be available when
         --  the loop exits.
         --
         nxt1 := next1;
         nxt2 := next2;
         --
         --  Get ready to examine the next fragment
         --
         next1 := string_table(nxt1).next;
         next2 := string_table(nxt2).next;
      end loop;
      if string_table(nxt1).len < string_table(nxt2).len then
         return CMP_LT;
      elsif string_table(nxt1).len > string_table(nxt2).len then
         return CMP_GT;
      end if;
      if (next1 > NIL_STR) and (next2 = NIL_STR) then
         return CMP_GT;
      elsif (next1 = NIL_STR) and (next2 > NIL_STR) then
         return CMP_LT;
      end if;
      return CMP_EQ;
   end;
   --
   --  Compare a Lisp string and an Ada String.  This loops through the two
   --  strings and compares character by character.  As soon as a character is
   --  not equal, it returns CMP_LT or CMP_GT.  If one string is longer than the
   --  other, but otherwise equal, the longer string is greater than the shorter one.
   --
   function compare(s1 : string_index; s2 : String) return comparison is
      next1 : string_index := s1;
      nxt1 : string_index;
      len1 : Integer := s2'First;
      s2_ptr : Integer := s2'First;
      limit : Integer;
   begin
      while next1 > NIL_STR loop
         len1 := s2'First;
         limit := string_table(next1).len;
         if (s2'Last - s2_ptr) < limit then
            limit := s2'Last - s2_ptr + 1;
         end if;
         for i in 1 .. limit loop
            if string_table(next1).str(i) < s2(s2_ptr) then
               return CMP_LT;
            elsif string_table(next1).str(i) > s2(s2_ptr) then
               return CMP_GT;
            end if;
            s2_ptr := s2_ptr + 1;
            len1 := len1 + 1;
         end loop;
         nxt1 := next1;
         next1 := string_table(nxt1).next;
      end loop;
      if (string_table(nxt1).len - len1) < (s2'Last - s2_ptr) then
         return CMP_LT;
      elsif (string_table(nxt1).len - len1) > (s2'Last - s2_ptr) then
         return CMP_GT;
      end if;
      if (next1 > NIL_STR) and (s2'Last = s2_ptr) then
         return CMP_GT;
      elsif (next1 = NIL_STR) and (s2'Last > s2_ptr) then
         return CMP_LT;
      end if;
      return CMP_EQ;
   end;
   --
   --  Returns the length of a string in characters
   --
   function length(s : string_index) return int32 is
      next : string_index := s;
      nxt : string_index;
      count : Natural := 0;
   begin
      while next > NIL_STR loop
         nxt := next;
         count := count + string_table(nxt).len;
         next := string_table(nxt).next;
      end loop;
      return int32(count);
   end;
   --
   --  Converts a fixed length Ada string to a Lisp string.  Returns false if
   --  the Lisp string cannot be allocated.
   --
   function str_to_lisp(s : out string_index; str : String) return Boolean is
      prev  : string_index;
      next  : string_index;
      first : string_index;
   begin
      if alloc(prev) then
         s := prev;
         first := prev;
         for ptr in str'First .. str'Last loop
            if string_table(prev).len < fragment_len then
               string_table(prev).len := string_table(prev).len + 1;
               string_table(prev).str(string_table(prev).len) := str(ptr);
            else
               if alloc(next) then
                  string_table(prev).next := next;
                  prev := next;
                  string_table(prev).len := 1;
                  string_table(prev).str(1) := str(ptr);
                  string_table(prev).next := NIL_STR;
               else
                  deref(first);
                  prev := NIL_STR;
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end if;
      s := NIL_STR;
      return False;
   end;
   --
   --  Functions to append to an existing string.
   --
   function append(s : string_index; c : Character) return Boolean is
      next : string_index := s;
      nxt : string_index;
      frag : string_index;
   begin
      while next > NIL_STR loop
         nxt := next;
         next := string_table(nxt).next;
      end loop;
      if string_table(nxt).len < fragment_len then
         string_table(nxt).len := string_table(nxt).len + 1;
         string_table(nxt).str(string_table(nxt).len) := c;
         return True;
      else
         if  alloc(frag) then
            string_table(frag).str(1) := c;
            string_table(frag).len := 1;
            string_table(nxt).next := frag;
            string_table(frag).next := NIL_STR;
            return True;
         else
            error("append", "Unable to allocate new string fragment.");
            return False;
         end if;
      end if;
   end;
   --
   --  Appends the string pointed to by str to the string pointed to by dest.
   --  Note that dest is updated to point to the last fragment in the string.
   --  This can be used to efficiently append multiple strings.  However, the
   --  pointer to the head of the destination string will need to be saved
   --  elsewhere.  Returns False if an error occurs.
   --
   function append(dest : in out string_index; str : string_index) return Boolean is
      next : string_index := string_table(dest).next;
      src_str  : string_index := str;
      temp_str : string_index := NIL_STR;
      dest_ptr : Integer;
      src_ptr  : integer;
   begin
      --
      --  Make sure that dest points to the last fragment.
      --
      while next > NIL_STR loop
         dest := next;
         next := string_table(dest).next;
      end loop;
      dest_ptr := string_table(dest).len + 1;
      --
      --  If the source doesn't point to a real string, then there is nothing
      --  more to do.
      if str = NIL_STR then
         return True;
      end if;
      --
      --  Check if the last fragment is full.  If so, add a new fragment.
      --
      if dest_ptr > fragment_len then
         if not alloc(temp_str) then
            error("append", "Unable to allocate string fragment");
            return False;
         end if;
         string_table(dest).next := temp_str;
         dest := temp_str;
         dest_ptr := 1;
      end if;
      src_ptr := 1;
      --
      --  Now copy the source string into the destination.
      --
      loop
         string_table(dest).str(dest_ptr) := string_table(src_str).str(src_ptr);
         string_table(dest).len := string_table(dest).len + 1;
         dest_ptr := dest_ptr + 1;
         src_ptr := src_ptr + 1;
         if (src_ptr > fragment_len) or (src_ptr > string_table(src_str).len) then
            src_str := string_table(src_str).next;
            src_ptr := 1;
         end if;
         exit when src_str = NIL_STR;
         --
         --  Add a new fragment when needed.
         --
         if dest_ptr > fragment_len then
            if not alloc(temp_str) then
               error("append", "Unable to allocate string fragment");
               return False;
            end if;
            string_table(dest).next := temp_str;
            dest := temp_str;
            dest_ptr := 1;
         end if;
      end loop;
      return True;
   end;
   --
   --  Convert a character to upper-case
   --
   function To_Upper(c : Character) return Character is
      u : Character;
   begin
      case c is
         when 'a' =>
            u := 'A';
         when 'b' =>
            u := 'B';
         when 'c' =>
            u := 'C';
         when 'd' =>
            u := 'D';
         when 'e' =>
            u := 'E';
         when 'f' =>
            u := 'F';
         when 'g' =>
            u := 'G';
         when 'h' =>
            u := 'H';
         when 'i' =>
            u := 'I';
         when 'j' =>
            u := 'J';
         when 'k' =>
            u := 'K';
         when 'l' =>
            u := 'L';
         when 'm' =>
            u := 'M';
         when 'n' =>
            u := 'N';
         when 'o' =>
            u := 'O';
         when 'p' =>
            u := 'P';
         when 'q' =>
            u := 'Q';
         when 'r' =>
            u := 'R';
         when 's' =>
            u := 'S';
         when 't' =>
            u := 'T';
         when 'u' =>
            u := 'U';
         when 'v' =>
            u := 'V';
         when 'w' =>
            u := 'W';
         when 'x' =>
            u := 'X';
         when 'y' =>
            u := 'Y';
         when 'z' =>
            u := 'Z';
         when others =>
            u := c;
      end case;
      return u;
   end;
   --
   --  Convert a character to lower-case
   --
   function To_Lower(c : Character) return Character is
      u : Character;
   begin
      case c is
         when 'A' =>
            u := 'a';
         when 'B' =>
            u := 'b';
         when 'C' =>
            u := 'c';
         when 'D' =>
            u := 'd';
         when 'E' =>
            u := 'e';
         when 'F' =>
            u := 'f';
         when 'G' =>
            u := 'g';
         when 'H' =>
            u := 'h';
         when 'I' =>
            u := 'i';
         when 'J' =>
            u := 'j';
         when 'K' =>
            u := 'k';
         when 'L' =>
            u := 'l';
         when 'M' =>
            u := 'm';
         when 'N' =>
            u := 'n';
         when 'O' =>
            u := 'o';
         when 'P' =>
            u := 'p';
         when 'Q' =>
            u := 'q';
         when 'R' =>
            u := 'r';
         when 'S' =>
            u := 's';
         when 'T' =>
            u := 't';
         when 'U' =>
            u := 'u';
         when 'V' =>
            u := 'v';
         when 'W' =>
            u := 'w';
         when 'X' =>
            u := 'x';
         when 'Y' =>
            u := 'y';
         when 'Z' =>
            u := 'z';
         when others =>
            u := c;
      end case;
      return u;
   end;
   --
   --  Copy helper function
   --
   function copy(s : string_index; t : transform) return element_type is
      source : string_index := s;
      new_frag : string_index;
      head : string_index;
      temp : string_index;
   begin
      if not alloc(head) then
         error("string copy", "Unable to allocate string fragment.");
         return make_error(ERR_UNKNOWN);
      end if;
      new_frag := head;
      string_table(new_frag).len := string_table(source).len;
      for index in 1 .. fragment_len loop
         case t is
            when NONE =>
               string_table(new_frag).str(index) := string_table(source).str(index);
            when UPPER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Upper(string_table(source).str(index));
            when LOWER =>
               string_table(new_frag).str(index) := BBS.lisp.strings.To_Lower(string_table(source).str(index));
         end case;
      end loop;
      source := string_table(source).next;
      while source /= string_index'First loop
         if not alloc(temp) then
            error("string copy", "Unable to allocate string fragment.");
            deref(head);
            return make_error(ERR_UNKNOWN);
         end if;
         string_table(new_frag).next := temp;
         new_frag := temp;
         string_table(new_frag).len := string_table(source).len;
         for index in 1 .. fragment_len loop
            case t is
            when NONE =>
               string_table(new_frag).str(index) := string_table(source).str(index);
            when UPPER =>
               string_table(new_frag).str(index) := To_Upper(string_table(source).str(index));
            when LOWER =>
               string_table(new_frag).str(index) := To_Lower(string_table(source).str(index));
            end case;
         end loop;
         source := string_table(source).next;
      end loop;
      return (kind => E_VALUE, v => (kind => V_STRING, s => head));
   end;
   --
   --  Functions for character positions.
   --
   --  Given a string index and an offset, follow the link list to find the
   --  fragment that contains the offset and position in that fragment.  If the
   --  offset is beyond the end of the string,  str is set to NIL_STR and
   --  the offset to 0.
   --
   procedure cannonicalize(str : in out string_index; offset : in out Natural) is
   begin
      if str > NIL_STR then
         while offset > string_table(str).len loop
            offset := offset - string_table(str).len;
            str := string_table(str).next;
            exit when str = NIL_STR;
         end loop;
      end if;
      if str = NIL_STR then
         offset := 0;
      end if;
   end;
   --
   --  Update a string index and offset in cannonical form to point to the next
   --  character in cannonical form.  If not in cannonical form, or if the next
   --  character is past end end of the string, str is set to NIL_STR and offset
   --  is set to 0.
   --
   procedure move_to_next_char(str : in out string_index; offset : in out Natural) is
   begin
      if str > NIL_STR then
         offset := offset + 1;
         if offset > string_table(str).len then
            offset := 1;
            str := string_table(str).next;
         end if;
      end if;
      if str = NIL_STR then
         offset := 0;
      end if;
   end;
   --
   --  Get a character at a cannonicalized position.  This must be a valid position.
   --
   function get_char_at(str : string_index; offset : Natural) return Character is
   begin
      return string_table(str).str(offset);
   end;
   --
   --  Parse a string as an integer.  Starts at the first character in the string
   --  and proceeds until either the end of the string fragment or an illegal
   --  character is found.
   --
   function parse_integer(str : string_index) return int32 is
      accumulate : int32 := 0;
      neg : Boolean := False;
      ptr : Integer;
   begin
      ptr := 1;
      if string_table(str).len > 0 then
         if string_table(str).str(ptr) = '-' then
            neg := true;
            ptr := ptr + 1;
         end if;
         while BBS.lisp.utilities.isDigit(string_table(str).str(ptr))
           and (ptr <= string_table(str).len) loop
            accumulate := accumulate*10 + int32'Value(" " & string_table(str).str(ptr));
            ptr := ptr + 1;
         end loop;
         if neg then
            accumulate := -accumulate;
         end if;
         return accumulate;
      else
         return 0;
      end if;
   end;
   --
   --  Get a substring of a string.  An ending offset of -1 means the end of the
   --  source string.
   --
   function substring(str : string_index; start_offset : Integer; len : Integer)
                      return string_index is
      source : string_index := str;
      head  : string_index;
      dest  : string_index;
      temp  : string_index;
      start : Integer := start_offset;
      count : Integer := len;
   begin
      if not alloc(head) then
         error("substring", "Unable to allocate string fragment.");
         return NIL_STR;
      end if;
      dest := head;
      loop
         --
         --  Fill destination fragment
         --
         for index in 1 .. fragment_len loop
            string_table(dest).str(index) := string_table(source).str(start);
            string_table(dest).len := string_table(dest).len + 1;
            if count /= -1 then
               count := count - 1;
            end if;
            exit when count = 0;
            start := start + 1;
            exit when (start > string_table(source).len) and (start <= fragment_len);
            if start > fragment_len then
               start := 1;
               source := string_table(source).next;
            end if;
            exit when source = NIL_STR;
         end loop;
         --
         --  Check if done
         --
         if count = 0 then
            return head;
         end if;
         if (start > string_table(source).len) and (start <= fragment_len) then
            return head;
         end if;
         --
         --  Allocate a new fragment for the destination
         --
         if not alloc(temp) then
            error("substring", "Unable to allocate string fragment.");
            deref(head);
            return NIL_STR;
         end if;
         string_table(dest).next := temp;
         dest := temp;
      end loop;
   end;
   --
   --  -------------------------------------------------------------------------
   --
   --  String iterator.  This can be used for looping through the characters in
   --  a string.  This is designed primarily to be used by BBS.lisp.parser.string.
   --
   --
   --  Initialize the buffer object
   --
   procedure init(self : in out str_iterator; s : string_index) is
   begin
      self.ptr := 1;
      self.base := s;
      self.current := s;
   end;
   --
   --  Move pointer to point to the next character in the string
   --
   procedure next_char(self : in out str_iterator) is
   begin
      move_to_next_char(self.current, self.ptr);
   end;
   --
   --
   --  Gets the character selected by ptr.
   --
   function get_char(self : str_iterator) return Character is
   begin
      if self.current > NIL_STR then
         if self.ptr <= string_table(self.current).len then
            return (string_table(self.current).str(self.ptr));
         end if;
      end if;
      return ' ';
   end;
   --
   --  Looks ahead to the next character after the current one.
   --
   function get_next_char(self : str_iterator) return Character is
      next : string_index;
   begin
      --
      --  Check easiest case first
      --
      if self.current = NIL_STR then
         return ' ';
      end if;
      --
      --  Now, we know that current points to an existing fragment.  Check if
      --  the next character is in the current fragment. (most common case)
      --
      if self.ptr < string_table(self.current).len then
         return string_table(self.current).str(self.ptr);
      end if;
      --
      -- Check if the current fragment is the last.  If so, there is no next character.
      --
      next := string_table(self.current).next;
      if next = NIL_STR then
         return ' ';
      end if;
      --
      --  Make sure that the length of the next fragment is greater than 0
      --
      if string_table(next).len = 0 then
         return ' ';
      end if;
      --
      -- Check the first character in the next fragment
      --
      return string_table(next).str(1);
   end;
   --
   --  Tests if the end of the string has been reached.
   --
   function is_end(self : str_iterator) return Boolean is
   begin
      return (self.current = NIL_STR) or else (self.ptr > string_table(self.current).len);
   end;
   --
   --  -------------------------------------------------------------------------
   --
   --  String memory management.
   --
   function count_free_str return Natural is
      count : Natural := 0;
   begin
      for i in string_table'Range loop
         if string_table(i).ref = 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end;
   --
   --  Find an unused string fragment, mark it as USED, and return the index
   --  in s.  Return false if no such cell could be found.
   --
   function alloc(s : out string_index) return Boolean is
   begin
      for i in string_table'Range loop
         if string_table(i).ref = FREE_STR then
            s := i;
            string_table(i).ref := FREE_STR + 1;
            string_table(i).len := 0;
            string_table(i).next := NIL_STR;
            return True;
         end if;
      end loop;
      s := NIL_STR;
      return False;
   end;
   --
   --  Increments the reference count of a string.
   --
   procedure ref(s : string_index) is
   begin
      if string_table(s).ref = FREE_STR then
         error("ref string", "Attempting to ref an unallocated string.");
      end if;
    string_table(s).ref := string_table(s).ref + 1;
   end;
   --
   --  Decrements the reference count of a string.
   --
   procedure deref(s : string_index) is
      next : string_index;
      prev : string_index;
   begin
      if string_table(s).ref > FREE_STR then
         string_table(s).ref := string_table(s).ref - 1;
      else
         error("deref string", "Attempt to deref an unreffed string at index "
              & string_index'Image(s));
      end if;
      --
      --  If the reference count goes to zero, deref the next fragment.
      --
      if string_table(s).ref = FREE_STR then
         prev := s;
         next := string_table(s).next;
         string_table(prev).len := 0;
         string_table(prev).next := NIL_STR;
         while next > NIL_STR loop
            if string_table(next).ref > FREE_STR then
               string_table(next).ref := string_table(next).ref - 1;
            else
               error("deref string", "Attempt to deref an unreffed string at index "
                     & string_index'Image(s));
            end if;
            exit when string_table(next).ref > FREE_STR;
            prev := next;
            next := string_table(prev).next;
            string_table(prev).len := 0;
            string_table(prev).next := NIL_STR;
         end loop;
      end if;
   end;
   --
   --  Reset the string table
   --
   procedure reset_string_table is
   begin
      for i in string_table'Range loop
         string_table(i).ref := FREE_STR;
      end loop;
   end;
   --
   --  Get the reference count for a string fragment.  Used primarily for
   --  debugging.
   --
   function ref_count(s : string_index) return str_ref_count is
   begin
      return string_table(s).ref;
   end;
   --
end;
