--
--  This package contains an assortment of Lisp predicates that can be used to
--  get information about various objects.  Most of these will be very simple
--  and some of them will return constant values because Tiny-Lisp doesn't
--  implement some things.
--
package BBS.lisp.evaluate.pred is
   --
   --  These return true of false depending on the type of data passed.
   --
   function nullp(e : element_type) return element_type;
   function symbolp(e : element_type) return element_type;
   function atomp(e : element_type) return element_type;
   function consp(e : element_type) return element_type;
   function listp(e : element_type) return element_type;
   function numberp(e : element_type) return element_type;
   function integerp(e : element_type) return element_type;
   function stringp(e : element_type) return element_type;
   function characterp(e : element_type) return element_type;
   function simple_string_p(e : element_type) return element_type;
   function functionp(e : element_type) return element_type;
   function compiled_function_p(e : element_type) return element_type;
   --
   --  These always return false as the data types are not implemented.
   --
   function return_false(e : element_type) return element_type;
--   function rationalp(e : element_type) return element_type;
--   function floatp(e : element_type) return element_type;
--   function realp(e : element_type) return element_type;
--   function complexp(e : element_type) return element_type;
--   function bit_vector_p(e : element_type) return element_type;
--   function vectorp(e : element_type) return element_type;
--   function simple_vector_p(e : element_type) return element_type;
--   function simple_bit_vector_p(e : element_type) return element_type;
--   function arrayp(e : element_type) return element_type;
--   function packagep(e : element_type) return element_type;

end;
