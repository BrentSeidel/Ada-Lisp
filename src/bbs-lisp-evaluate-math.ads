--
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   pragma Elaborate_Body;
   --
   --  Perform addition
   --
   procedure add(e : out element_type; s : cons_index);
   --
   --  Perform subtraction
   --
   procedure sub(e : out element_type; s : cons_index);
   --
   --  Perform multiplication
   --
   procedure mul(e : out element_type; s : cons_index);
   --
   --  Perform division
   --
   procedure div(e : out element_type; s : cons_index);

private
   --
   --  Helper function for math operations.
   --
   function eval_math(s : cons_index; b : mathops) return element_type;
end;
