--
--  This package contains functions for evaluating the various Lisp operatations.
--  Note that some groups of functions have been moved into sub-packages of this
--  one.
--
package BBS.lisp.evaluate is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function quit(e : element_type) return element_type;
   function dump(e : element_type) return element_type;
   function reset(e : element_type) return element_type;
   function quote(e : element_type) return element_type;
   function msg_on(e : element_type) return element_type;
   function msg_off(e : element_type) return element_type;
   --
end;
