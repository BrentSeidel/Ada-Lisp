--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
with BBS.lisp.strings;
with BBS.lisp.memory;
package body BBS.lisp.stack is
   --
   --
   --  Adding and removing items from the stack
   --
   procedure pop(v : out stack_entry) is
      t : stack_entry := (kind => ST_EMPTY);
   begin
      if not isEmpty then
         t := stack(stack_pointer);
         if t.kind = ST_VALUE then
            BBS.lisp.memory.deref(t.st_name);
            BBS.lisp.memory.deref(t.st_value);
         end if;
         stack(stack_pointer) := (kind => ST_EMPTY);
         stack_pointer := stack_pointer - 1;
      else
         error("pop", "Stack underflow");
      end if;
      v := t;
   end;
   --
   procedure push(v : stack_entry) is
   begin
      if not isFull then
         if v.kind = ST_VALUE then
            BBS.lisp.memory.ref(v.st_name);
         end if;
         stack_pointer := stack_pointer + 1;
         stack(stack_pointer) := v;
      else
         error("push", "Stack overflow");
      end if;
   end;
   --
   --  Procedure for clearing stack.  This is done at the command line level.
   --  There should be nothing on the stack at this point.  Some error conditions
   --  may cause a return to the command line without clearing the stack.
   --
   procedure reset is
      temp : stack_entry;
   begin
      while not isEmpty loop
         pop(temp);
         if temp.kind = ST_VALUE then
            BBS.lisp.memory.deref(temp.st_name);
            BBS.lisp.memory.deref(temp.st_value);
         end if;
      end loop;
      stack_pointer := EMPTY_STACK;
      frame_pointer := EMPTY_STACK;
      temp_frame := EMPTY_STACK;
      frame_count := 0;
   end;
   --
   --  Operations for stack frames
   --
   procedure start_frame is
   begin
      frame_count := frame_count + 1;
      push((kind => ST_FRAME, number => frame_count, next => frame_pointer));
      temp_frame := stack_pointer;
   end;
   --
   procedure enter_frame is
   begin
      frame_pointer := temp_frame;
      temp_frame := EMPTY_STACK;
   end;
   --
   procedure exit_frame is
      frame : constant stack_entry := stack(frame_pointer);
   begin
      if frame.kind /= ST_FRAME then
         error("exit_frame", "Not a stack frame.");
         return;
      end if;
      for temp in frame_pointer .. stack_pointer loop
         if stack(temp).kind = ST_VALUE then
            BBS.lisp.memory.deref(stack(temp).st_name);
            BBS.lisp.memory.deref(stack(temp).st_value);
         end if;
         stack(temp) := (kind => ST_EMPTY);
      end loop;
      stack_pointer := frame_pointer - 1;
      frame_pointer := frame.next;
      frame_count := frame.number - 1;
   end;
   --
   procedure dump is
      e : stack_entry;
   begin
      put_line("Stack dump start");
      Put_Line("SP: " & stack_index'Image(stack_pointer) & ", FP: " &
                 stack_index'Image(frame_pointer));
      for i in stack_index'First + 1 .. stack_index'Last loop
         e := stack(i);
         case e.kind is
            when ST_EMPTY =>
               null;
            when ST_FRAME =>
               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
               put(", Number => " & Natural'Image(e.number));
               put_line(", Next => " & stack_index'Image(e.next));
            when ST_VALUE =>
               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
               put(", Name => ");
               print(e.st_name);
               put(", Value => ");
               print(e.st_value);
               new_line;
--            when others =>
--               put_line(stack_index'Image(i) & " Unknown stack entry kind.");
         end case;
      end loop;
      put_line("Stack dump end");
   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, a value of none is returned.
   --
   function search_frames(offset : Natural; name : string_index) return value is
      frame : stack_index := frame_pointer;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > EMPTY_STACK loop
         test := stack(stack_index(Integer(frame) + Integer(offset)));
         if test.kind = ST_VALUE then
            test_name := test.st_name;
         end if;
         if test.kind /= ST_EMPTY then
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               if test.kind = ST_VALUE then
                  return test.st_value;
               else
                  error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
                  put("Searching for variable <");
                  print(name);
                  Put_Line(">");
                  dump;
                  frame := EMPTY_STACK;
               end if;
            end if;
         end if;
         if stack(frame).kind = ST_FRAME then
            frame := stack(frame).next;
         else
            error("search_frames", "Did not find frame entry on stack");
            put("Searching for variable <");
            print(name);
            Put_Line(">");
            dump;
            frame := EMPTY_STACK;
         end if;
      end loop;
      return (kind => V_NONE);
   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(offset : Natural; name : string_index) return stack_index is
      frame : stack_index := frame_pointer;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > EMPTY_STACK loop
         test := stack(stack_index(Integer(frame) + Integer(offset)));
         if test.kind = ST_VALUE then
            test_name := test.st_name;
         end if;
         if test.kind /= ST_EMPTY then
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               if test.kind = ST_VALUE then
                  return stack_index(Integer(frame) + Integer(offset));
               else
                  error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
                  put("Searching for variable <");
                  print(name);
                  Put_Line(">");
                  dump;
                  frame := EMPTY_STACK;
               end if;
            end if;
         end if;
         if stack(frame).kind = ST_FRAME then
            frame := stack(frame).next;
         else
            error("search_frames", "Did not find frame entry on stack");
            dump;
            frame := EMPTY_STACK;
         end if;
      end loop;
      return stack_index'First;
   end;
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(name : string_index; index : out stack_index) return Natural is
      sp : stack_index := stack_pointer;
      fp : stack_index := frame_pointer;
      item  : stack_entry;
      eq : comparison := CMP_NE;
   begin
      while sp > EMPTY_STACK loop
         item := stack(sp);
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
         return Natural(sp - fp);
      else
         index := EMPTY_STACK;
         return Natural'First;
      end if;
   end;
   --

end;
