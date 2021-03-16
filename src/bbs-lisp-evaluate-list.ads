package BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   procedure cons(e : out element_type; s : cons_index);
   --
   --  Get the first item of a list
   --
   procedure car(e : out element_type; s : cons_index);
   --
   --  Get the rest of a list
   --
   procedure cdr(e : out element_type; s : cons_index);
   --
   --  Create a list verbatum from the parameter list
   --
   procedure quote(e : out element_type; s : cons_index);
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   procedure list(e : out element_type; s : cons_index);
   --
   --  Append one list to another (currently unimplemented).
   --
--   procedure append(e : out element_type; s : cons_index);
end;
