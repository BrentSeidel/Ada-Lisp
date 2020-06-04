with bbs.lisp.utilities;
package bbs.lisp.evaluate is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function car(e : element_type) return element_type;
   function cdr(e : element_type) return element_type;
   function setq(e : element_type) return element_type;
   function eval_if(e : element_type) return element_type;
   function print(e : element_type) return element_type;
   function dowhile(e : element_type) return element_type;
   function dotimes(e : element_type) return element_type;
   function defun(e : element_type) return element_type;
   function add(e : element_type) return element_type;
   function sub(e : element_type) return element_type;
   function mul(e : element_type) return element_type;
   function div(e : element_type) return element_type;
   function quit(e : element_type) return element_type;
   function eq(e : element_type) return element_type;
   function ne(e : element_type) return element_type;
   function lt(e : element_type) return element_type;
   function gt(e : element_type) return element_type;
   function true(e : element_type) return element_type;
   function dump(e : element_type) return element_type;
   function reset(e : element_type) return element_type;
   function quote(e : element_type) return element_type;
   function newline(e : element_type) return element_type;
   function msg_on(e : element_type) return element_type;
   function msg_off(e : element_type) return element_type;
   function read_line(e : element_type) return element_type;
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
