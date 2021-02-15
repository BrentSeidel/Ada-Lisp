package BBS.lisp.evaluate.misc is
   --
   --  Functions for evaluating the various builtin functions.
   --
   --
   --  Exit the lisp interpreter.
   --
--   function quit(s : cons_index) return element_type;
   procedure quit(e : out element_type; s : cons_index);
   --
   --  Dump some of the internal tables for debugging purposes.
   --
--   function dump(s : cons_index) return element_type;
   procedure dump(e : out element_type; s : cons_index);
   --
   --  Turn debugging messages on or off.
   --
--   function msg(s : cons_index) return element_type;
   procedure msg(e : out element_type; s : cons_index);
   --
   --  Pause for the specified number of milliseconds.
   --
--   function sleep(s : cons_index) return element_type;
   procedure sleep(e : out element_type; s : cons_index);
end;
