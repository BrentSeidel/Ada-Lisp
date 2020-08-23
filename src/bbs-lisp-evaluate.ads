--
--  This package contains helper functions for evaluating the various Lisp
--  operatations.  The actual operations are in subpackages of this one.
--
package BBS.lisp.evaluate is
   --
   --  Execute the statements in a block and return the value of the last
   --  statement executed.
   --
   function execute_block(e : element_type) return element_type;
   --
end;
