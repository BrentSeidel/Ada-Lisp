with bbs.lisp.utilities;
private package bbs.lisp.evaluate is
   --
   --  Function for dispatching the various functions for evaluation.
   --
   function eval_dispatch(s : cons_index) return element_type;
   --
private
   --
   --  Functions for evaluating the various builtin functions.
   --
   function eval_math(e : element_type; b : builtins) return element_type;
   function eval_car(e : element_type) return element_type;
   function eval_cdr(e : element_type) return element_type;
   function eval_comp(e : element_type; b : builtins) return element_type;
   function eval_setq(e : element_type) return element_type;
   function eval_if(e : element_type) return element_type;
   function eval_print(e : element_type) return element_type;
   function eval_dowhile(e : element_type) return element_type;
   function eval_dotimes(e : element_type) return element_type;
   function eval_defun(e : element_type) return element_type;
   --
   --  Functions for evaluating lisp functions
   --
   function eval_function(s : cons_index; e : element_type) return element_type;
end;
