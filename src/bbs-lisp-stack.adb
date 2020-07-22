--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
with BBS.lisp.strings;
package body BBS.lisp.stack is
   --
   --  Status functions for the stack
   --
   function isFull return Boolean is
   begin
      return (stack_pointer = stack_index'Last);
   end;
   --
   function isEmpty return Boolean is
   begin
      return (stack_pointer = stack_index'First);
   end;
   --
   --
   --  Adding and removing items from the stack
   --
   function pop return stack_entry is
      t : stack_entry := (kind => ST_EMPTY);
   begin
      if not isEmpty then
         t := stack(stack_pointer);
         stack(stack_pointer) := (kind => ST_EMPTY);
         stack_pointer := stack_pointer - 1;
      else
         error("pop", "Stack underflow");
      end if;
      return t;
   end;
   --
   procedure push(v : stack_entry) is
   begin
      if not isFull then
         stack_pointer := stack_pointer + 1;
         stack(stack_pointer) := v;
      else
         error("push", "Stack overflow");
      end if;
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
      temp_frame := 0;
   end;
   --
   procedure exit_frame is
      temp : stack_index := frame_pointer;
      frame : stack_entry := stack(frame_pointer);
   begin
      if frame.kind /= ST_FRAME then
         error("exit_frame", "Not a stack frame.");
         return;
      end if;
      for temp in frame_pointer .. stack_pointer loop
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
            when ST_PARAM =>
               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
               put(", Name => ");
               print(e.p_name);
               put(", Value => ");
               print(e.p_value);
               new_line;
            when ST_LOCAL =>
               put(stack_index'Image(i) & " " & stack_entry_type'Image(e.kind));
               put(", Name => ");
               print(e.l_name);
               put(", Value => ");
               print(e.l_value);
               new_line;
            when others =>
               put_line(stack_index'Image(i) & " Unknown stack entry kind.");
         end case;
      end loop;
      put_line("Stack dump end");
   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, a value of none is returned.
   --
   function search_frames(offset : stack_index; name : string_index) return value is
      frame : stack_index := frame_pointer;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > 0 loop
         test := stack(frame + offset);
         if test.kind = ST_PARAM then
            test_name := test.p_name;
         elsif test.kind = ST_LOCAL then
            test_name := test.l_name;
--         else
            --
            -- This might be OK if the variable is not in the current frame.
            --
--            error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
--            put("Searching for variable <");
--            print(name);
--            Put_Line(">");
--            dump;
--            frame := 0;
         end if;
         if test.kind /= ST_EMPTY then
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               if test.kind = ST_PARAM then
                  return test.p_value;
               elsif test.kind = ST_LOCAL then
                  return test.l_value;
               else
                  error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
                  put("Searching for variable <");
                  print(name);
                  Put_Line(">");
                  dump;
                  frame := 0;
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
            frame := 0;
         end if;
      end loop;
      return (kind => V_NONE);
   end;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(offset : stack_index; name : string_index) return stack_index is
      frame : stack_index := frame_pointer;
      test : stack_entry;
      test_name : string_index;
      eq : comparison;
   begin
      while frame > 0 loop
         test := stack(frame + offset);
         if test.kind = ST_PARAM then
            test_name := test.p_name;
         elsif test.kind = ST_LOCAL then
            test_name := test.l_name;
--         else
--            error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
--            dump;
--            frame := 0;
         end if;
         if test.kind /= ST_EMPTY then
            eq := BBS.lisp.strings.compare(name, test_name);
            if eq = CMP_EQ then
               if (test.kind = ST_PARAM) or (test.kind = ST_LOCAL) then
                  return frame + offset;
               else
                  error("search_frames", "Found unexpected entry type " & stack_entry_type'Image(test.kind));
                  put("Searching for variable <");
                  print(name);
                  Put_Line(">");
                  dump;
                  frame := 0;
               end if;
            end if;
         end if;
         if stack(frame).kind = ST_FRAME then
            frame := stack(frame).next;
         else
            error("search_frames", "Did not find frame entry on stack");
            dump;
            frame := 0;
         end if;
      end loop;
      return 0;
   end;
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(name : string_index; index : out stack_index) return stack_index is
      sp : stack_index := stack_pointer;
      fp : stack_index := frame_pointer;
      item  : stack_entry;
      eq : comparison := CMP_NE;
   begin
--      Put_Line("find_offset: Starting sp: " & stack_index'Image(sp) & ", fp: " &
--                 stack_index'Image(fp));
      while sp > 0 loop
         item := stack(sp);
--         Put_Line("find_offset: Checking item of kind " & stack_entry_type'Image(item.kind));
         case item.kind is
            when ST_FRAME =>
               fp := item.next;
            when ST_LOCAL =>
               eq := BBS.lisp.strings.compare(name, item.l_name);
            when ST_PARAM =>
               eq := BBS.lisp.strings.compare(name, item.p_name);
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
         index := 0;
         return 0;
      end if;
   end;
   --

end;
