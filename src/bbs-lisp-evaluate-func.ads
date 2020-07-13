--
--  This package contains functions to support Lisp function definition and
--  evaluation.
--
package BBS.lisp.evaluate.func is
   function defun(e : element_type; p : phase) return element_type;
   --
   --  Functions for evaluating lisp functions
   --
   function eval_function(s : cons_index; e : element_type) return element_type;
end;
