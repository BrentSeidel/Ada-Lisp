package BBS.lisp.evaluate.bool is
   --
   --  Perform a logical NOT operation.
   --
--   function eval_not(s : cons_index) return element_type;
   procedure eval_not(e : out element_type; s : cons_index);
   --
   --  Perform a logical AND operation.
   --
--   function eval_and(s : cons_index) return element_type;
   procedure eval_and(e : out element_type; s : cons_index);
   --
   --  Perform a logical OR operation.
   --
--   function eval_or(s : cons_index) return element_type;
   procedure eval_or(e : out element_type; s : cons_index);
end;
