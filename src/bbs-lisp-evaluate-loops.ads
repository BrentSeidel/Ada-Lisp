--
--  This package contains the functions for Lisp loop operations.
--
package BBS.lisp.evaluate.loops is
   --
   --  Evaluate statements while a condition is true.
   --
   function dowhile(s : cons_index) return element_type;
   --
   --  Evaluate statements a specified number of times.
   --
   procedure dotimes(e : out element_type; s : cons_index; p : phase);
end;
