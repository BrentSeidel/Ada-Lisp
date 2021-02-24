--
--  This package contains the functions for Lisp loop operations.
--
package BBS.lisp.evaluate.loops is
   --
   --  Evaluate statements while a condition is true.
   --
   procedure dowhile(e : out element_type; s : cons_index);
   --
   --  Evaluate statements a specified number of times.
   --
   procedure dotimes(e : out element_type; s : cons_index; p : phase);
   --
   --  Create a block containing multiple statements
   --
   procedure progn(e : out element_type; s : cons_index);
   --
   --  Breaks out of a loop (or other exclosing block) and returns a value
   --
   procedure return_from(e : out element_type; s : cons_index);
end;
