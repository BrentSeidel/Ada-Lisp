--
--  This package contains the console I/O routines.  They are fairly basic.
--
package BBS.lisp.evaluate.io is
   function print(e : element_type) return element_type;
   function fresh_line(e : element_type) return element_type;
   function read_line(e : element_type) return element_type;
   function terpri(e : element_type) return element_type;
end;
