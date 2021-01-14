--
--  This package contains the Lisp comparison and condition functions
--
package BBS.lisp.evaluate.cond is
   --
   --  Compare two items for equality.
   --
   function eq(s : cons_index) return element_type;
   --
   --  Compare two items for not equality.
   --
   function ne(s : cons_index) return element_type;
   --
   --  Is first item less than the second item?
   --
   function lt(s : cons_index) return element_type;
   --
   --  Is the first item greater than the second item?
   --
   function gt(s : cons_index) return element_type;
   --
   --  Perform an IF operation.
   --
   function eval_if(s : cons_index) return element_type;
private
   --
   --  Various helper functions
   --
   function eval_comp(s : cons_index; b : compops) return element_type;
end;
