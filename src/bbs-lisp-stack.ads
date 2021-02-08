--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
package BBS.lisp.stack is
   --
   --  Data types for the stack
   --
   type stack_entry_type is (ST_EMPTY, ST_FRAME, ST_VALUE);
   type stack_entry(kind : stack_entry_type := ST_EMPTY) is
      record
         case kind is
            when ST_EMPTY =>
               null;
            when ST_FRAME =>
               number: Natural;
               next : stack_index;
            when ST_VALUE =>
               st_name : string_index;
               st_value : value;
         end case;
      end record;
   --
   --  The stack array
   --
   stack : array (stack_index'First + 1 .. stack_index'Last) of stack_entry :=
     (others => (kind => ST_EMPTY));
   --
   --  Various pointers for managing the stack and its frames.
   --
   stack_pointer : stack_index := stack_index'First;
   frame_pointer : stack_index := stack_index'First;
   temp_frame    : stack_index := stack_index'First;
   frame_count   : Natural := 0;
   --
   --  Status functions for the stack
   --
   function isFull return Boolean
     with Global => (Input => stack_pointer);
   function isEmpty return Boolean
     with Global => (Input => stack_pointer);
   --
   --  Adding and removing items from the stack
   --
   function pop return stack_entry
     with Global => (Input => (stack, stack_pointer));
   --  Should be (In_Out => (stack, stack_pointer));
   procedure push(v : stack_entry)
     with Global => (In_Out => (stack, stack_pointer));
   --
   --  Operations for stack frames.  The usage is as follows:
   --  1) Call start_frame before pushing items onto the stack that should be
   --     in the frame.
   --  2) Call enter_frame once the items are all on the stack.  This finalizes
   --     stack frame creation.
   --  3) Call exit_frame to clean up the stack frame.  There is no need to pop
   --     the items off the stack that belong to the stack frame.
   --
   procedure start_frame
     with Global => (Input => frame_pointer,
                     In_Out => (stack, stack_pointer, frame_count),
                     Output => temp_frame);
   procedure enter_frame
     with Global => (In_Out => temp_frame, Output => frame_pointer);
   procedure exit_frame
     with Global => (In_out => (stack, stack_pointer, frame_pointer),
                     Output => frame_count);
   --
   --  Procedure for clearing stack.  This is done at the command line level.
   --  There should be nothing on the stack at this point.  Some error conditions
   --  may cause a return to the command line without clearing the stack.
   --
   procedure reset
     with Global => (Output => (stack, stack_pointer, frame_pointer, temp_frame, frame_count)),
     Post => ((stack_pointer = stack_index'First) and (frame_pointer = stack_index'First) and
              (temp_frame = stack_index'First) and (frame_count = 0));
   --
   --  Dump the stack for debugging purposes
   --
   procedure dump
     with Global => (Input => (stack, stack_pointer, frame_pointer));
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, an empty value is returned.
   --
   function search_frames(offset : Natural; name : string_index) return value
     with Global => (Input => (stack, frame_pointer));
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(offset : Natural; name : string_index) return stack_index
     with Global => (Input => (stack, frame_pointer));
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(name : string_index; index : out stack_index) return Natural
     with Global => (Input => (stack, stack_pointer, frame_pointer));
end;

