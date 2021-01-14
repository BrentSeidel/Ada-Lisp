package BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   function cons(s : cons_index) return element_type;
   --
   --  Get the first item of a list
   --
   function car(s : cons_index) return element_type;
   --
   --  Get the rest of a list
   --
   function cdr(s : cons_index) return element_type;
   --
   --  Create a list verbatum from the parameter list
   --
   function quote(s : cons_index) return element_type;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   function list(s : cons_index) return element_type;
   --
   --  Append one list to another (currently unimplemented).
   --
   function append(s : cons_index) return element_type;
end;
