--
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   --
   --  Perform addition
   --
   function add(s : cons_index) return element_type;
   --
   --  Perform subtraction
   --
   function sub(s : cons_index) return element_type;
   --
   --  Perform multiplication
   --
   function mul(s : cons_index) return element_type;
   --
   --  Perform division
   --
   function div(s : cons_index) return element_type;
private
   --
   --  Various helper functions
   --
   function eval_math(s : cons_index; b : mathops) return element_type;
end;
