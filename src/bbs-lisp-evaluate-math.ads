--
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   --
   --  Helper function for math operations.
   --
   function eval_math(s : cons_index; b : mathops) return element_type;
   --
   --  Perform addition
   --
   function add(s : cons_index) return element_type is (eval_math(s, PLUS))
      with Inline;
   --
   --  Perform subtraction
   --
   function sub(s : cons_index) return element_type is (eval_math(s, MINUS))
      with Inline;
   --
   --  Perform multiplication
   --
   function mul(s : cons_index) return element_type is (eval_math(s, MUL))
      with Inline;
   --
   --  Perform division
   --
   function div(s : cons_index) return element_type is (eval_math(s, DIV))
      with Inline;

--private
end;
