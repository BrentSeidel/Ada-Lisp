--
--  This package contains the data structures and routines for accessing the
--  symbol table.
--
package BBS.lisp.symbols is
   --
   --  A symbol give a perminant name to a piece of data.  These can be global
   --  variables, user defined functions, or builtin functions.  The builtin
   --  functions are predefined and cannot be changed.
   --  A symbol record contains a name and a type
   --
   type symbol(kind : symbol_type := SY_EMPTY) is
      record
         ref : Natural;
         str : string_index;
         case kind is
            when SY_SPECIAL =>
               s : special_function;
            when SY_BUILTIN =>
               f : execute_function;
            when SY_LAMBDA =>
               ps : cons_index;
            when SY_VARIABLE =>
               pv : element_type;
            when SY_EMPTY =>
               null;
         end case;
      end record;
   --
   --  Operations for symbols
   --
   --  Get the type of the symbol
   --
   function get_type(s : symb_index) return symbol_type
     with pre => (s /= NIL_SYM);
   --
   --  Check if symbol is fixed (builtin or special)
   --
   function isFixed(s : symb_index) return Boolean
     with pre => (s /= NIL_SYM);
   --
   --  Check if symbol is a function (builtin, special, or user defined function)
   --
   function isFunction(s : symb_index) return Boolean
     with pre => (s /= NIL_SYM);
   --
   --  If symbol is a variable, get the symbol value.
   --
   function get_value(s : symb_index) return element_type
     with pre => (get_type(s) = SY_VARIABLE);
   --
   --  If symbol is a lambda, get the list.
   --
   function get_list(s : symb_index) return cons_index
     with pre => (get_type(s) = SY_LAMBDA);
   --
   --  Get a symbol's name
   --
   function get_name(s : symb_index) return string_index
     with pre => (s /= NIL_SYM);
   --
   --  Get a symbol's reference count
   --
   function get_ref(s : symb_index) return Natural
     with pre => (s /= NIL_SYM);
   --
   --  Get a symbol from the symbol table
   --
   function get_sym(s : symb_index) return symbol
     with pre => (s /= NIL_SYM);
   --
   --  Set a symbol entry
   --
   procedure set_sym(s : symb_index; val : symbol)
     with pre => (s /= NIL_SYM);
   --
   --  Reset the symbol table
   --
   procedure reset_symbol_table;
   --
private
   --
   --  The symbol table.
   --
   symb_table : array (symb_index'First + 1 .. symb_index'Last) of symbol;
   --
end;
