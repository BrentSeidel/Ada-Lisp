package BBS.lisp.evaluate.symb is
   --
   --  Coerces an object of one type to another type.  Available coercions are:
   --    character -> string
   --    boolean -> string
   --    boolean -> integer (NIL -> 0, T -> 1)
   --    integer -> boolean (0 -> NIL, /= 0 -> T)
   --
   procedure coerce(e : out element_type; s : cons_index);
   --
   --  Concatenate two strings or lists.
   --
   procedure concatenate(e : out element_type; s : cons_index);

private
   --
   --  The first time one of these functions is called, populate the symbol
   --  indices so the symbol table doesn't have to be searched each time.
   --
   not_initialized : Boolean := True;
   sym_bool : symb_index := NIL_SYM;
   sym_char : symb_index := NIL_SYM;
   sym_int  : symb_index := NIL_SYM;
   sym_list : symb_index := NIL_SYM;
   sym_str  : symb_index := NIL_SYM;
--   sym_bool : symbol_ptr := (kind => ST_NULL);
--   sym_char : symbol_ptr := (kind => ST_NULL);
--   sym_int  : symbol_ptr := (kind => ST_NULL);
--   sym_list : symbol_ptr := (kind => ST_NULL);
--   sym_str  : symbol_ptr := (kind => ST_NULL);
   --
   --  Initialize the symbol indices.  Return true if successful or false it not.
   --
   function init_syms return Boolean;
end;
