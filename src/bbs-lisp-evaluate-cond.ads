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
--   function eq(s : cons_index) return element_type is (eval_comp(s, SYM_EQ))
--     with Inline;
   procedure eq(e : out element_type; s : cons_index);
   --
   --  Compare two items for not equality.
   --
--   function ne(s : cons_index) return element_type is (eval_comp(s, SYM_NE))
--     with Inline;
   procedure ne(e : out element_type; s : cons_index);
   --
   --  Is first item less than the second item?
   --
--   function lt(s : cons_index) return element_type is (eval_comp(s, SYM_LT))
--     with Inline;
   procedure lt(e : out element_type; s : cons_index);
   --
   --  Is the first item greater than the second item?
   --
--   function gt(s : cons_index) return element_type is (eval_comp(s, SYM_GT))
--     with Inline;
   procedure gt(e : out element_type; s : cons_index);
   --
   --  Perform an IF operation.
   --
--   function eval_if(s : cons_index) return element_type;
   procedure eval_if(e : out element_type; s : cons_index);
end;
