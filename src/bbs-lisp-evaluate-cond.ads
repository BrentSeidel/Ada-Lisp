--
--  This package contains the Lisp comparison and condition functions
--
package BBS.lisp.evaluate.cond is
   function eq(e : element_type) return element_type;
   function ne(e : element_type) return element_type;
   function lt(e : element_type) return element_type;
   function gt(e : element_type) return element_type;
   function eval_if(e : element_type) return element_type;
private
   --
   --  Various helper functions
   --
   function eval_comp(e : element_type; b : compops) return element_type;
end;
