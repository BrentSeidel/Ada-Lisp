package BBS.lisp.evaluate.list is

   --
   --  Create a list out of two elements.
   --
   function cons(e : element_type) return element_type;
   --
   --  Get the first item of a list
   --
   function car(e : element_type) return element_type;
   --
   --  Get the rest of a list
   --
   function cdr(e : element_type) return element_type;
   --
   --  Create a list verbatum from the parameter list
   --
   function quote(e : element_type) return element_type;
   --
   --  Create a list by evaluating the parameters, similar to quote, but quote
   --  does not evaluate the parameters.
   --
   function list(e : element_type) return element_type;
   --
   --  Append one list to another.
   --
   function append(e : element_type) return element_type;
end;
