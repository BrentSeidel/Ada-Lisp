with bbs.lisp.utilities;
package bbs.lisp.evaluate is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function eval_car(e : element_type) return element_type;
   function eval_cdr(e : element_type) return element_type;
   function eval_setq(e : element_type) return element_type;
   function eval_if(e : element_type) return element_type;
   function eval_print(e : element_type) return element_type;
   function eval_dowhile(e : element_type) return element_type;
   function eval_dotimes(e : element_type) return element_type;
   function eval_defun(e : element_type) return element_type;
   function eval_add(e : element_type) return element_type;
   function eval_sub(e : element_type) return element_type;
   function eval_mul(e : element_type) return element_type;
   function eval_div(e : element_type) return element_type;
   function eval_quit(e : element_type) return element_type;
   function eval_eq(e : element_type) return element_type;
   function eval_ne(e : element_type) return element_type;
   function eval_lt(e : element_type) return element_type;
   function eval_gt(e : element_type) return element_type;
   function eval_true(e : element_type) return element_type;
   function eval_dump(e : element_type) return element_type;
   function eval_reset(e : element_type) return element_type;
   function eval_quote(e : element_type) return element_type;
   function eval_newline(e : element_type) return element_type;
   --
   --  Functions for evaluating lisp functions
   --
   function eval_function(s : cons_index; e : element_type) return element_type;
   --
private
   --
   --  Various helper functions
   --
   function eval_math(e : element_type; b : mathops) return element_type;
   function eval_comp(e : element_type; b : compops) return element_type;
end;
