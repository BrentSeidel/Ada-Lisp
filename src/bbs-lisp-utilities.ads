package bbs.lisp.utilities is
   --
   --  Various utility functions
   --
   function count(s : cons_index) return Integer;
   function isTrue(e : element_type) return Boolean;
   function isList(e : element_type) return Boolean;
   function isFunction(e : element_type) return Boolean;
   function getList(e : element_type) return cons_index
     with pre => isList(e);
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
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, an element type pointing to the value.  If not, the element
   --  points to the original atom.
   --
   function indirect_elem(e : element_type) return element_type;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   procedure first_value(e : element_type; car : out element_type; cdr : out element_type);
end;
