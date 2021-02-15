--
--  This package contains the console I/O routines.  They are fairly basic.
--
package BBS.lisp.evaluate.io is
   --
   --  Print a list of items.
   --
--   function print(s : cons_index) return element_type;
   procedure print(e : out element_type; s : cons_index);
   --
   --  Print a new line if not already at the beginning of a line.
   --
--   function fresh_line(s : cons_index) return element_type;
   procedure fresh_line(e : out element_type; s : cons_index);
   --
   --  Read a line from input.
   --
--   function read_line(s : cons_index) return element_type;
   procedure read_line(e : out element_type; s : cons_index);
   --
   --  Print a new line.
   --
--   function terpri(s : cons_index) return element_type;
   procedure terpri(e : out element_type; s : cons_index);
end;
