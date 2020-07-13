--
--  This package contains the various Lisp math functions.
--
package BBS.lisp.evaluate.math is
   function add(e : element_type) return element_type;
   function sub(e : element_type) return element_type;
   function mul(e : element_type) return element_type;
   function div(e : element_type) return element_type;
private
   --
   --  Various helper functions
   --
   function eval_math(e : element_type; b : mathops) return element_type;
end;
