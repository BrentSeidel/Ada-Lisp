package bbs.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer;
   --
   --  The following routine supports parameters and local variables.
   --  It scans through the passed s expression (recursively, if necessary) and
   --  when it finds a symbol or tempsym, it looks through the list of passed
   --  in parameters or local variables.  If the name matches, it replaces the
   --  symbol or tempsym with a pointer to the parameter or local variable and
   --  updates the ref count.  The return value is the number of replacements
   --  made.
   --
   function replace_syms(s : cons_index; lib : cons_index) return Natural;
   --
   --  Perform the replacement for a single symbol/variable
   --
   function replace_sym(s : cons_index; var : element_type) return Natural;
   --
   --  Function to determine if a character is a digit or not in different number
   --  systems.
   --
   function isDigit(c : Character) return Boolean;
   function isAlpha(c : Character) return Boolean;
   function isHex(c : Character) return Boolean;
   function hexDigit(c : Character) return uint32;
end;
