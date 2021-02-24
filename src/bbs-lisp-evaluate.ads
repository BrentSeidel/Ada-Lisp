--
--  This package contains helper functions for evaluating the various Lisp
--  operatations.  The actual operations are in subpackages of this one.
--
with BBS.lisp.stack;
package BBS.lisp.evaluate
with Abstract_State =>  pvt_exit_block is
   --
   function isTrue(e : element_type) return Boolean
     with Global => (Input => cons_table);
   function isList(e : element_type) return Boolean
     with Global => Null;
   function isFunction(e : element_type) return Boolean
     with Global => (Input => cons_table);
   function getList(e : element_type) return cons_index
     with post => (if not isList(e) then getList'Result = NIL_CONS else
                       getList'Result in cons_index'Range),
       global => Null;
   --
   --  Execute the statements in a block and return the value of the last
   --  statement executed.
   --
   function execute_block(e : element_type) return element_type
     with Global => (input => (cons_table, symb_table, pvt_string_table,
                               pvt_exit_flag, pvt_break_flag, pvt_msg_flag,
                               pvt_exit_block, pvt_first_char_flag,
                               BBS.lisp.stack.pvt_stack,
                               BBS.lisp.stack.pvt_sp));
--                               BBS.lisp.stack.frame_pointer));
   --  should be (In_Out => (cons_table, symb_table, pvt_string_table,
   --                        pvt_exit_flag, pvt_break_flag, pvt_msg_flag,
   --                        pvt_exit_loop, pvt_first_char_flag,
   --                        BBS.lisp.stack.stack,
   --                        BBS.lisp.stack.stack_pointer,
   --                        BBS.lisp.stack.frame_pointer)
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, an element type pointing to the value.  If not, the element
   --  points to the original atom.
   --
   function indirect_elem(e : element_type) return element_type
     with Global => (input => (pvt_string_table, symb_table,
                               BBS.lisp.stack.pvt_stack,
                               BBS.lisp.stack.pvt_sp));
--                               BBS.lisp.stack.frame_pointer));
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   function first_value(s : in out cons_index) return element_type;
   --
   --  Set the exit_loop flag
   --
   procedure set_exit_block(n : Natural)
     with Global => (Output => pvt_exit_block),
     Inline;
   --
   --  Decrement the exit_block flag
   --
   procedure decrement_exit_block
     with Global => (Output => pvt_exit_block);
   --
   --  Returns the exit_block flag
   --
   function get_exit_block return Natural
     with Global => (Input => pvt_exit_block),
     Inline;
   --
private
      --
   --  Set to non-zero to break out of that many nested loops
   --
   exit_block : Natural := 0
     with Part_Of => pvt_exit_block;

end;
