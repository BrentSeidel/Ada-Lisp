--
--  This package contains debugging routines for the lisp interpreter.  In normal
--  operations, these won't be used and these package can be deleted.
--
package BBS.lisp.debug is
   procedure dump(e : element_type)
     with Global => (Input => (cons_table));
   procedure dump(s : cons_index)
     with Global => (Input => (cons_table));
   procedure dump(s : symbol_ptr)
     with Global => (Input => (cons_table));
   procedure dump(v : value)
     with Global => (Input => (cons_table));
end;
