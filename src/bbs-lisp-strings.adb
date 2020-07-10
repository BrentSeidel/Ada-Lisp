--with Ada.Characters.Handling;
with BBS.lisp.memory;
package body bbs.lisp.strings is
   --
   --  Converts a string to upper-case in place.
   --
   procedure uppercase(s : string_index) is
      next : string_index := s;
      nxt : string_index;
   begin
      while (next >= (string_index'First + 1))
        and (next <= string_index'Last) loop
         nxt := next;
         for i in 1 .. string_table(nxt).len loop
            string_table(nxt).str(i) := To_Upper(string_table(nxt).str(i));
         end loop;
         next := string_table(nxt).next;
      end loop;
   end;
   --
   --  Procedure to print a string
   --
   --
   --  Converts a string to lower-case in place.
   --
   procedure lowercase(s : string_index) is
      next : string_index := s;
      nxt : string_index;
   begin
      while (next >= (string_index'First + 1))
        and (next <= string_index'Last) loop
         nxt := next;
         for i in 1 .. string_table(nxt).len loop
            string_table(nxt).str(i) := To_Lower(string_table(nxt).str(i));
         end loop;
         next := string_table(nxt).next;
      end loop;
   end;
   --
   --  Compare two strings.  This loops through the two strings and compares
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
      while (next1 >= (string_index'First + 1))
        and (next1 <= string_index'Last)
        and (next2 >= (string_index'First + 1))
        and (next2 <= string_index'Last) loop
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
--      put_line("Checking fragment 1 index " & string_index'Image(nxt1) &
--         " and fragment 2 index " & string_index'Image(nxt2));
      if string_table(nxt1).len < string_table(nxt2).len then
         return CMP_LT;
      elsif string_table(nxt1).len > string_table(nxt2).len then
         return CMP_GT;
      end if;
      if (next1 >= (string_index'First + 1))
        and (next1 <= string_index'Last)
        and ((next2 < (string_index'First + 1))
        or (next2 > string_index'Last)) then
         return CMP_GT;
      elsif ((next1 < (string_index'First + 1))
        or (next1 > string_index'Last))
        and (next2 >= (string_index'First + 1))
        and (next2 <= string_index'Last) then
         return CMP_LT;
      end if;
      return CMP_EQ;
   end;
   --
   --  Returns the length of a string in characters
   --
   function length(s : string_index) return Natural is
      next : string_index := s;
      nxt : string_index;
      count : Natural := 0;
   begin
      while (next >= (string_index'First + 1))
        and (next <= string_index'Last) loop
         nxt := next;
         count := count + string_table(nxt).len;
         next := string_table(nxt).next;
      end loop;
      return count;
   end;
   --
   --  Converts a fixed length Ada string to a Lisp string.  Returns false if
   --  the Lisp string cannot be allocated.
   --
   function str_to_lisp(s : out string_index; str : String) return Boolean is
      prev  : string_index;
      next : string_index;
      first : string_index;
      flag : Boolean;
   begin
      flag := bbs.lisp.memory.alloc(prev);
      string_table(prev).len := 0;
      string_table(prev).next := -1;
      s := prev;
      first := prev;
      if flag then
         for ptr in str'First .. str'Last loop
            if string_table(prev).len < fragment_len then
               string_table(prev).len := string_table(prev).len + 1;
               string_table(prev).str(string_table(prev).len) := str(ptr);
            else
               flag := bbs.lisp.memory.alloc(next);
               if flag then
                  string_table(prev).next := next;
                  prev := next;
                  string_table(prev).len := 1;
                  string_table(prev).str(1) := str(ptr);
                  string_table(prev).next := -1;
               else
                  bbs.lisp.memory.deref(first);
                  return False;
               end if;
            end if;
         end loop;
      end if;
      return True;
   end;
   --
   --  Functions to append to an existing string.
   --
   function append(s : string_index; c : Character) return Boolean is
      next : string_index := s;
      nxt : string_index;
      count : Natural := 0;
      flag : Boolean;
      frag : string_index;
   begin
      while (next >= (string_index'First + 1))
        and (next <= string_index'Last) loop
         nxt := next;
         count := count + string_table(nxt).len;
         next := string_table(nxt).next;
      end loop;
      if string_table(nxt).len < fragment_len then
         string_table(nxt).len := string_table(nxt).len + 1;
         string_table(nxt).str(string_table(nxt).len) := c;
         return True;
      else
         flag := BBS.lisp.memory.alloc(frag);
         if flag then
            string_table(frag).str(1) := c;
            string_table(frag).len := 1;
            string_table(nxt).next := frag;
            string_table(frag).next := -1;
            return True;
         else
            error("append", "Unable to allocate new string fragment.");
            return False;
         end if;
      end if;
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
end;
