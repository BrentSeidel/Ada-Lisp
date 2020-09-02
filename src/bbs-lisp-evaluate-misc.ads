package BBS.lisp.evaluate.misc is
   --
   --  Functions for evaluating the various builtin functions.
   --
   function quit(e : element_type) return element_type;
   function dump(e : element_type) return element_type;
   function reset(e : element_type) return element_type;
   function msg(e : element_type) return element_type;
   function sleep(e : element_type) return element_type;
end;
