--
--  This package contains debugging routines for the lisp interpreter.  In normal
--  operations, these won't be used and these package can be deleted.
--
package BBS.lisp.debug is
   procedure dump(e : element_type);
   procedure dump(s : cons_index);
   procedure dump(s : symbol_ptr);
end;
