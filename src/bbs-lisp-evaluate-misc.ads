package BBS.lisp.evaluate.misc is
   --
   --  Functions for evaluating the various builtin functions.
   --
   --
   --  Exit the lisp interpreter.
   --
   procedure quit(e : out element_type; s : cons_index);
   --
   --  Dump some of the internal tables for debugging purposes.
   --
   procedure dump(e : out element_type; s : cons_index);
   --
   --  Turn debugging messages on or off.
   --
   procedure msg(e : out element_type; s : cons_index);
   --
   --  Pause for the specified number of milliseconds.
   --
   procedure sleep(e : out element_type; s : cons_index);
end;
