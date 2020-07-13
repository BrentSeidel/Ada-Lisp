--
--  This package contains functions for evaluating the various Lisp operatations.
--  Note that some groups of functions have been moved into sub-packages of this
--  one.
--
package BBS.lisp.evaluate is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function car(e : element_type) return element_type;
   function cdr(e : element_type) return element_type;
   function setq(e : element_type; p : phase) return element_type;
   function print(e : element_type) return element_type;
   function quit(e : element_type) return element_type;
   function dump(e : element_type) return element_type;
   function reset(e : element_type) return element_type;
   function quote(e : element_type) return element_type;
   function newline(e : element_type) return element_type;
   function msg_on(e : element_type) return element_type;
   function msg_off(e : element_type) return element_type;
   function read_line(e : element_type) return element_type;
   --
end;
