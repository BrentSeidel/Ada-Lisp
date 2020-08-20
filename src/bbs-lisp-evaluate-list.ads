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
end;
