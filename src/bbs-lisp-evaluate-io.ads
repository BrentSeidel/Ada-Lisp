--
--  This package contains the console I/O routines.  They are fairly basic.
--
package BBS.lisp.evaluate.io is
   pragma Elaborate_Body;
   --
   --  Print a list of items.
   --
   procedure print(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
   --
   --  Print a new line if not already at the beginning of a line.
   --
   procedure fresh_line(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
   --
   --  Read a line from input.
   --
   procedure read_line(e : out element_type; s : cons_index);
   --
   --  Read a line from a string and parse it.
   --
   procedure read_expr(e : out element_type; s : cons_index);
   --
   --  Print a new line.
   --
   procedure terpri(e : out element_type; s : cons_index)
     with post => (e = NIL_ELEM);
end;
