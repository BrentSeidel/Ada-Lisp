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
               next : Natural;
            when ST_VALUE =>
               st_name : string_index;
               st_value : value;
         end case;
      end record;
   --
   type lisp_stack_array is array (Natural range <>) of stack_entry;
   type lisp_stack(size : Natural) is tagged record
      sp : Natural;  -- Stack pointer
      fp : Natural;  -- Frame pointer
      fc : Natural;  -- Frame counter
      stack : lisp_stack_array (0 .. size);
   end record;
   --
   --
   --  Status functions for the stack
   --
   function isFull(self : lisp_stack) return Boolean;
   --
   --  Adding and removing items from the stack
   --
   procedure push(self : in out lisp_stack; name : string_index; val : value; err : out Boolean);
   --
   --  Operations for stack frames.  The usage is as follows:
   --  1) Call start_frame before pushing items onto the stack that should be
   --     in the frame.
   --  2) Call enter_frame once the items are all on the stack.  This finalizes
   --     stack frame creation.
   --  3) Call exit_frame to clean up the stack frame.  There is no need to pop
   --     the items off the stack that belong to the stack frame.
   --
   procedure start_frame(self : in out lisp_stack; err : out Boolean);
   procedure exit_frame(self : in out lisp_stack);
   --
   --  Procedure for clearing stack.  This is done at the command line level.
   --  There should be nothing on the stack at this point.  Some error conditions
   --  may cause a return to the command line without clearing the stack.
   --
--   procedure reset
--     with Global => (Output => (pvt_stack, pvt_sp, pvt_fp, pvt_fc)),
--     Post => (isEmpty);
   --
   --  Dump the stack for debugging purposes.  Since the stack is private, this
   --  needs to be defined here instead of in BBS.lisp.debug.  Uncomment to use.
   --
--   procedure dump
--     with Global => (Input => (pvt_stack, pvt_sp, pvt_fp));
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, an empty value is returned.
   --
   function search_frames(self : lisp_stack; offset : Natural; name : string_index) return value;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the stack index of the variable is returned, if not 0 is returned.
   --
   function search_frames(self : lisp_stack; offset : Natural; name : string_index) return Natural;
   --
   --  Searches the stack to find a variable and returns the stack index and offset
   --
   function find_offset(self : lisp_stack; name : string_index; index : out Natural) return Natural;
   --
   -- Returns the frame pointer so that it can be private.
   --
   function get_fp(self : lisp_stack) return Natural;
   --
   --  Sets an entry on the stack
   --
   procedure set_entry(self : in out lisp_stack; e : Natural; v : stack_entry; err : out Boolean);
   --
   --  Sets the value of an entry on the stack
   --
   procedure set_value(self : in out lisp_stack; e : Natural; v : value; err : out Boolean);
   --
   --  Gets an entry from the stack
   --
   function get_entry(self : in out lisp_stack; e : Natural; err : out Boolean) return stack_entry;
   --
end;

