--
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   --
   --  Perform addition
   --
--   function add(s : cons_index) return element_type is (eval_math(s, PLUS))
--      with Inline;
   procedure add(e : out element_type; s : cons_index);
   --
   --  Perform subtraction
   --
--   function sub(s : cons_index) return element_type is (eval_math(s, MINUS))
--      with Inline;
   procedure sub(e : out element_type; s : cons_index);
   --
   --  Perform multiplication
   --
--   function mul(s : cons_index) return element_type is (eval_math(s, MUL))
--      with Inline;
   procedure mul(e : out element_type; s : cons_index);
   --
   --  Perform division
   --
--   function div(s : cons_index) return element_type is (eval_math(s, DIV))
--      with Inline;
   procedure div(e : out element_type; s : cons_index);

private
   --
   --  Helper function for math operations.
   --
   function eval_math(s : cons_index; b : mathops) return element_type;
end;
