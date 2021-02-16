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
   procedure atomp(e : out element_type; s : cons_index);
   procedure characterp(e : out element_type; s : cons_index);
   procedure compiled_function_p(e : out element_type; s : cons_index);
   procedure consp(e : out element_type; s : cons_index);
   procedure errorp(e : out element_type; s : cons_index);
   procedure functionp(e : out element_type; s : cons_index);
   procedure integerp(e : out element_type; s : cons_index);
   procedure listp(e : out element_type; s : cons_index);
   procedure nullp(e : out element_type; s : cons_index);
   procedure numberp(e : out element_type; s : cons_index);
   procedure simple_string_p(e : out element_type; s : cons_index);
   procedure stringp(e : out element_type; s : cons_index);
   procedure symbolp(e : out element_type; s : cons_index);
   --
   --  These always return false as the data types are not implemented.
   --
   procedure return_false(e : out element_type; s : cons_index);
--   procedure arrayp(e : out element_type; s : cons_index);
--   procedure bit_vector_p(e : out element_type; s : cons_index);
--   procedure complexp(e : out element_type; s : cons_index);
--   procedure floatp(e : out element_type; s : cons_index);
--   procedure rationalp(e : out element_type; s : cons_index);
--   procedure realp(e : out element_type; s : cons_index);
--   procedure packagep(e : out element_type; s : cons_index);
--   procedure simple_vector_p(e : out element_type; s : cons_index);
--   procedure simple_bit_vector_p(e : out element_type; s : cons_index);
--   procedure vectorp(e : out element_type; s : cons_index);

end;
