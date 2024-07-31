--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of Tiny-Lisp.
--  Tiny-Lisp is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  Tiny-Lisp is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.--
--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
with BBS.lisp.strings;
with BBS.lisp.memory;
package body BBS.lisp.stack is
   --
   function isFull(self : lisp_stack) return Boolean is
   begin
      return (self.sp = self.size);
   end;
   --
   --  Adding items to the stack.  Popping actually isn't done.  Items are removed
   --  from the stack as part of the exit_frame.
   --
   --
   procedure push(self : in out lisp_stack; name : string_index; val : element_type; err : out Boolean) is
   begin
      if not self.isFull then
         BBS.lisp.strings.ref(name);
         BBS.lisp.memory.ref(val);
         self.sp := self.sp + 1;
         self.stack(self.sp) := (kind => ST_VALUE, st_name => name, st_value => val);
         err := False;
      else
         error("Push", "Stack overflow");
         err := True;
      end if;
   end;
   --
   --  Procedure for clearing stack.  This is done at the command line level.
   --  There should be nothing on the stack at this point.  Some error conditions
   --  may cause a return to the command line without clearing the stack.
   --
--   procedure reset is
--      temp : stack_entry;
--   begin
--      while not isEmpty loop
--         pop(temp);
--         if temp.kind = ST_VALUE then
--            BBS.lisp.memory.deref(temp.st_name);
--            BBS.lisp.memory.deref(temp.st_value);
--         end if;
--      end loop;
--      stack_pointer := EMPTY_STACK;
--      frame_pointer := EMPTY_STACK;
--      frame_count := 0;
--   end;
   --
   --  Operations for stack frames
   --
   --  Start a stack frame
   --
   procedure start_frame(self : in out lisp_stack; err : out Boolean) is
   begin
      if not self.isFull then
         self.fc := self.fc + 1;
         self.sp := self.sp + 1;
         self.stack(self.sp) := (kind => ST_FRAME, number => self.fc, next => self.fp);
         self.fp := self.sp;
         err := False;
      else
         error("Start_frame", "Stack overflow");
         err := True;
      end if;
   end;
   --
   --  Exit a stack frame
   --
   procedure exit_frame(self : in out lisp_stack) is
      frame : constant stack_entry := self.stack(self.fp);
   begin
      if frame.kind /= ST_FRAME then
         error("exit_frame", "Not a stack frame.");
         put_line("exit_frame: Frame pointer is: " & Natural'Image(self.fp));
         put_line("exit_frame: Stack entry type is: " & stack_entry_type'Image(frame.kind));
         return;
      end if;
      for temp in self.fp .. self.sp loop
         if self.stack(temp).kind = ST_VALUE then
            BBS.lisp.strings.deref(self.stack(temp).st_name);
            BBS.lisp.memory.deref(self.stack(temp).st_value);
         end if;
         self.stack(temp) := (kind => ST_EMPTY);
      end loop;
      self.sp := self.fp - 1;
      self.fp := frame.next;
      self.fc := frame.number - 1;
   end;
   --
   --  Returns the frame pointer
   --
   function get_fp(self : lisp_stack) return Natural is
   begin
      return self.fp;
   end;
   --
   --  Sets an entry on the stack
   --
   procedure set_entry(self : in out lisp_stack; e : Natural; v : stack_entry; err : out Boolean) is
   begin
      if e <= self.sp then
         self.stack(e) := v;
         err := False;
      else
         error("set_entry", "Stack index out of range");
         err := True;
      end if;
   end;
   --
   --  Sets the value of an entry on the stack
   --
   procedure set_value(self : in out lisp_stack; e : Natural; v : element_type; err : out Boolean) is
   begin
      if e <= self.sp then
         if self.stack(e).kind = ST_VALUE then
            self.stack(e).st_value := v;
            err := False;
         else
            error("set_value", "Entry is not a value type");
            err := True;
         end if;
      else
         error("set_value", "Stack index out of range");
         err := True;
      end if;
   end;
   --
   --  Gets an entry from the stack
   --
   function get_entry(self : in out lisp_stack; e : Natural; err : out Boolean) return stack_entry is
   begin
      if e <= self.sp then
         err := False;
         return self.stack(e);
      else
         err := True;
         error("get_entry", "Index out of range.");
         return (kind => ST_EMPTY);
      end if;
   end;
   --
--   procedure dump is
--      e : stack_entry;
--   begin
--      put_line("Stack dump start");
--      Put_Line("SP: " & stack_index'Image(stack_pointer) & ", FP: " &
--                 stack_index'Image(frame_pointer));
--      for i in stack_index'First .. stack_index'Last loop
--         e := stack(i);
--         case e.kind is
--            when ST_EMPTY =>
--               null;
--            when ST_FRAME =>
--               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
--               put(", Number => " & Natural'Image(e.number));
--               put_line(", Next => " & stack_index'Image(e.next));
--            when ST_VALUE =>
--               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
--               put(", Name => ");
--               print(e.st_name);
--               put(", Value => ");
--               print(e.st_value);
--               new_line;
----            when others =>  -- Unused for now.  May be used if other kinds are added.
----               put_line(stack_index'Image(i) & " Unknown stack entry kind.");
--         end case;
--      end loop;
--      put_line("Stack dump end");
--   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, a value of none is returned.
   --
   --
   function search_frames(self : lisp_stack; offset : Natural; name : string_index) return element_type is
      frame : Natural := self.fp;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > Natural'First loop
         if frame + offset > self.size then
            error("search frames", "Stack pointer value out of range");
            return (Kind => V_NONE);
         end if;
         test := self.stack(frame + offset);
         if test.kind = ST_VALUE then
            test_name := test.st_name;
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               return test.st_value;
            end if;
         end if;
         if self.stack(frame).kind = ST_FRAME then
            frame := self.stack(frame).next;
         else
            error("search_frames", "Did not find frame entry on stack");
            put("Searching for variable <");
            print(name);
            Put_Line(">");
            frame := Natural'First;
         end if;
      end loop;
      return (kind => V_NONE);
   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(self : lisp_stack; offset : Natural; name : string_index) return Natural is
      frame : Natural := self.fp;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > Natural'First loop
         if frame + offset > self.size then
            error("search frames", "Stack pointer value out of range");
            return Natural'First;
         end if;
         test := self.stack(frame + offset);
         if test.kind = ST_VALUE then
            test_name := test.st_name;
         end if;
         if test.kind /= ST_EMPTY then
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               if test.kind = ST_VALUE then
                  return frame + offset;
               else
                  error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
                  put("Searching for variable <");
                  print(name);
                  Put_Line(">");
                  frame := Natural'First;
               end if;
            end if;
         end if;
         if self.stack(frame).kind = ST_FRAME then
            frame := self.stack(frame).next;
         else
            error("search_frames", "Did not find frame entry on stack");
            frame := Natural'First;
         end if;
      end loop;
      return Natural'First;
   end;
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(self : lisp_stack; name : string_index; index : out Natural) return Natural is
      sp : Natural := self.sp;
      fp : Natural := self.fp;
      item  : stack_entry;
      eq : comparison := CMP_NE;
   begin
      while sp > Natural'First loop
         item := self.stack(sp);
         case item.kind is
            when ST_FRAME =>
               fp := item.next;
            when ST_VALUE =>
               eq := BBS.lisp.strings.compare(name, item.st_name);
            when others =>
               null;
         end case;
         exit when eq = CMP_EQ;
         sp := sp - 1;
      end loop;
      if eq = CMP_EQ then
         index := sp;
         return sp - fp;
      else
         index := Natural'First;
         return Natural'First;
      end if;
   end;
   --
end;
