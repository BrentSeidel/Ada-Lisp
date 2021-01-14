package BBS.lisp.evaluate.misc is
   --
   --  Functions for evaluating the various builtin functions.
   --
   --
   --  Exit the lisp interpreter.
   --
   function quit(s : cons_index) return element_type;
   --
   --  Dump some of the internal tables for debugging purposes.
   --
   function dump(s : cons_index) return element_type;
   --
   --  Calls initialization to restart the interpreter.  Note that this does not
   --  do any external initialization that may be required in an embedded
   --  application.
   --
   function reset(s : cons_index) return element_type;
   --
   --  Turn debugging messages on or off.
   --
   function msg(s : cons_index) return element_type;
   --
   --  Pause for the specified number of milliseconds.
   --
   function sleep(s : cons_index) return element_type;
end;
