package BBS.lisp.evaluate.misc is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function quit(e : element_type) return element_type;
   function dump(e : element_type) return element_type;
   function reset(e : element_type) return element_type;
   function msg_on(e : element_type) return element_type;
   function msg_off(e : element_type) return element_type;
end;
