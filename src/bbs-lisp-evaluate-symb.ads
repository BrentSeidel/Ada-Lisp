package BBS.lisp.evaluate.symb is
   --
   --  Coerces an object of one type to another type.  Available coercions are:
   --    character -> string
   --    boolean -> string
   --    boolean -> integer (NIL -> 0, T -> 1)
   --    integer -> boolean (0 -> NIL, /= 0 -> T)
   --
   function coerce(e : element_type) return element_type;
   function concatenate(e : element_type) return element_type;

private
   --
   --  The first time one of these functions is called, populate the symbol
   --  indices so the symbol table doesn't have to be searched each time.
   --
   not_initialized : Boolean := True;
   sym_char : symb_index := -1;
   sym_str  : symb_index := -1;
   sym_bool : symb_index := -1;
   sym_int  : symb_index := -1;
   --
   --  Initialize the symbol indices.  Return true if successful or false it not.
   --
   function init_syms return Boolean;
end;
