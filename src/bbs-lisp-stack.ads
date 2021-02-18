--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
package BBS.lisp.stack
with Abstract_State => (pvt_stack, pvt_sp, pvt_fp, pvt_fc) is
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
   EMPTY_STACK : constant stack_index := stack_index'First;
   FULL_STACK  : constant stack_index := stack_index'Last;
   --
   --  Status functions for the stack
   --
   function isFull return Boolean
     with Global => (Input => pvt_sp),
     Inline;
   function isEmpty return Boolean
     with Global => (Input => pvt_sp),
     Inline;
   --
   --  Adding and removing items from the stack
   --
   procedure pop(v : out stack_entry)
     with Global => (In_Out => (pvt_stack, pvt_sp)),
     pre => not isEmpty,
     post => not isFull;
   procedure push(v : stack_entry)
     with Global => (In_Out => (pvt_stack, pvt_sp)),
     pre => not isFull,
     post => not isEmpty;
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
     with Global => (In_Out => (pvt_stack, pvt_sp, pvt_fc, pvt_fp));
   procedure exit_frame
     with Global => (In_out => (pvt_stack, pvt_sp, pvt_fp, pvt_fc));
   --
   --  Procedure for clearing stack.  This is done at the command line level.
   --  There should be nothing on the stack at this point.  Some error conditions
   --  may cause a return to the command line without clearing the stack.
   --
   procedure reset
     with Global => (Output => (pvt_stack, pvt_sp, pvt_fp, pvt_fc)),
     Post => (isEmpty);
   --
   --  Dump the stack for debugging purposes
   --
   procedure dump
     with Global => (Input => (pvt_stack, pvt_sp, pvt_fp));
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, an empty value is returned.
   --
   function search_frames(offset : Natural; name : string_index) return value
     with Global => (Input => (pvt_stack, pvt_fp));
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(offset : Natural; name : string_index) return stack_index
     with Global => (Input => (pvt_stack, pvt_fp));
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(name : string_index; index : out stack_index) return Natural
     with Global => (Input => (pvt_stack, pvt_sp, pvt_fp));
   --
   -- Returns the frame pointer so that it can be private.
   --
   function get_fp return stack_index
     with Global => (Input => pvt_fp),
     Inline;
   --
   --  Sets an entry on the stack
   --
   procedure set_entry(e : stack_index; v : stack_entry)
     with Global => (Output => pvt_stack);
   --
   --  Sets the value of an entry on the stack
   --
   procedure set_value(e : stack_index; v : value)
     with Global => (Output => pvt_stack);
   --
   --  Gets an entry from the stack
   --
   function get_entry(e : stack_index) return stack_entry
     with Global => (Input => pvt_stack),
     Inline;
   --
private
   --
   --  The stack array
   --
   stack : array (stack_index'First .. stack_index'Last) of stack_entry :=
     (others => (kind => ST_EMPTY))
       with Part_Of => pvt_stack;
   --
   --  Various pointers for managing the stack and its frames.
   --
   stack_pointer : stack_index := stack_index'First
     with Part_Of => pvt_sp;
   frame_pointer : stack_index := stack_index'First
     with Part_Of => pvt_fp;
   frame_count   : Natural := 0
     with Part_Of => pvt_fc;

end;

