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
--  This package contains helper functions for evaluating the various Lisp
--  operatations.  The actual operations are in subpackages of this one.
--
package BBS.lisp.evaluate
with Abstract_State =>  pvt_exit_block is
   --
   --  Various utility functions
   --
   function isTrue(e : element_type) return Boolean;
   function isList(e : element_type) return Boolean
     with Global => Null;
   function isFunction(e : element_type) return Boolean;
   function getList(e : element_type) return cons_index
     with post => (if not isList(e) then getList'Result = NIL_CONS else
                       getList'Result'Valid),
       global => Null;
   function makeList(s : cons_index) return element_type
     with Global => Null;
   --
   --  Execute the statements in a block and return the value of the last
   --  statement executed.
   --
   function execute_block(s : cons_index) return element_type
     with Global => (input => (pvt_exit_flag, pvt_break_flag, pvt_msg_flag,
                               pvt_exit_block, pvt_first_char_flag));
   --  should be (In_Out => (pvt_exit_flag, pvt_break_flag, pvt_msg_flag,
   --                        pvt_exit_loop, pvt_first_char_flag)
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, an element type pointing to the value.  If not, the element
   --  points to the original atom.
   --
   function indirect_elem(e : element_type) return element_type;
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
