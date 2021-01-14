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
   function atomp(s : cons_index) return element_type;
   function characterp(s : cons_index) return element_type;
   function compiled_function_p(s : cons_index) return element_type;
   function consp(s : cons_index) return element_type;
   function functionp(s : cons_index) return element_type;
   function integerp(s : cons_index) return element_type;
   function listp(s : cons_index) return element_type;
   function nullp(s : cons_index) return element_type;
   function numberp(s : cons_index) return element_type;
   function simple_string_p(s : cons_index) return element_type;
   function stringp(s : cons_index) return element_type;
   function symbolp(s : cons_index) return element_type;
   --
   --  These always return false as the data types are not implemented.
   --
   function return_false(s : cons_index) return element_type;
--   function arrayp(e : cons_index) return element_type;
--   function bit_vector_p(e : cons_index) return element_type;
--   function complexp(e : cons_index) return element_type;
--   function floatp(e : cons_index) return element_type;
--   function rationalp(e : cons_index) return element_type;
--   function realp(e : cons_index) return element_type;
--   function packagep(e : cons_index) return element_type;
--   function simple_vector_p(e : cons_index) return element_type;
--   function simple_bit_vector_p(e : cons_index) return element_type;
--   function vectorp(e : cons_index) return element_type;

end;
