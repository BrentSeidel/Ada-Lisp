--
--  This package contains functions and procedures for managing the stack for
--  the Lisp interpreter.
--
package BBS.lisp.stack is
   --
   --  Data types for the stack
   --
   type stack_entry_type is (ST_EMPTY, ST_FRAME, ST_PARAM, ST_LOCAL);
   type stack_entry(kind : stack_entry_type := ST_EMPTY) is
      record
         case kind is
            when ST_EMPTY =>
               null;
            when ST_FRAME =>
               number: Natural;
               next : stack_index;
            when ST_PARAM =>
               p_name : string_index;
               p_value : value;
               when ST_LOCAL =>
               l_name : string_index;
               l_value : value;
         end case;
      end record;
   --
   --  The stack array
   --
   stack : array (stack_index) of stack_entry;
   --
   --  Various pointers for managing the stack and its frames.
   --
   stack_pointer : stack_index := 0;
   frame_pointer : stack_index := 0;
   frame_count   : Natural := 0;
   --
   --  Status functions for the stack
   --
   function isFull return Boolean;
   function isEmpty return Boolean;
   --
   --  Adding and removing items from the stack
   --
   function pop return stack_entry;
   procedure push(v : stack_entry);
   --
   --  Operations for stack frames
   --
   procedure enter_frame;
   procedure exit_frame;
   --
   --  Dump the stack for debugging purposes
   --
   procedure dump;
   --
   --  Search stack for the variable.  The frame offset and name are used to
   --  look backwards through the stack frames for a match to the name.  If
   --  found, the value is returned.  If not found, an empty value is returned.
   --
   function search_frames(offset : stack_index; name : string_index) return value;
end;
