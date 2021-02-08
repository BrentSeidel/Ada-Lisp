package bbs.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer
     with Global => (Input => cons_table);
   --
   --  The following routine supports parameters and local variables.
   --  It scans through the passed s expression (recursively, if necessary) and
   --  when it finds a symbol, it looks at the passed in parameter or local
   --  variable.  If the name matches, it replaces the symbol with a pointer to
   --  the parameter or local variable and updates the ref count.  The return
   --  value is the number of replacements made.
   --
   --  Perform the replacement for a single symbol/variable.  Searches the list
   --  s and any symbols or tempsyms whose name matches that of var are replaced
   --  by var.  This means that stack variables will shadow symbols.
   --
   function replace_sym(s : cons_index; var : element_type) return Natural
     with Global => (Input => (cons_table, symb_table));
   -- Really should be (Input => symb_table, In_Out => cons_table).
   --
   --  Function to determine if a character is a digit or not in different number
   --  systems.
   --
   function isDigit(c : Character) return Boolean
     with Global => Null;
   function isAlpha(c : Character) return Boolean
     with Global => Null;
   function isHex(c : Character) return Boolean
     with Global => Null;
   function hexDigit(c : Character) return uint32
     with Global => Null;
end;
