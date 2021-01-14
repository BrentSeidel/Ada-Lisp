package BBS.lisp.evaluate.bool is
   --
   --  Perform a logical NOT operation.
   --
   function eval_not(s : cons_index) return element_type;
   --
   --  Perform a logical AND operation.
   --
   function eval_and(s : cons_index) return element_type;
   --
   --  Perform a logical OR operation.
   --
   function eval_or(s : cons_index) return element_type;
end;
