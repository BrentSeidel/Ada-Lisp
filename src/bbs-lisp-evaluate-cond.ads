--
--  This package contains the Lisp comparison and condition functions
--
package BBS.lisp.evaluate.cond is
   --
   --  Helper function for comparisons
   --
   function eval_comp(s : cons_index; b : compops) return element_type;
   --
   --  Compare two items for equality.
   --
   procedure eq(e : out element_type; s : cons_index);
   --
   --  Compare two items for not equality.
   --
   procedure ne(e : out element_type; s : cons_index);
   --
   --  Is first item less than the second item?
   --
   procedure lt(e : out element_type; s : cons_index);
   --
   --  Is the first item greater than the second item?
   --
   procedure gt(e : out element_type; s : cons_index);
   --
   --  Perform an IF operation.
   --
   procedure eval_if(e : out element_type; s : cons_index);
   --
   --  Perform a COND operation.
   --
   procedure eval_cond(e : out element_type; s : cons_index);
end;
