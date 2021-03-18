--
--  This package contains debugging routines for the lisp interpreter.  In normal
--  operations, these won't be used and these package can be deleted.
--
package bbs.lisp.debug is
   procedure dump(e : element_type)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(s : cons_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(s : symb_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(v : value)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
end;
