package body BBS.lisp.symbols is
   --
   --  Operations for symbols
   --
   --  Get the type of the symbol
   --
   function get_type(s : symb_index) return symbol_type is
   begin
      return symb_table(s).kind;
   end;
   --
   --  Check if symbol is fixed (builtin or special)
   --
   function isFixed(s : symb_index) return Boolean is
      t : constant symbol_type := symb_table(s).kind;
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL);
   end;
   --
   --  Check if symbol is a function (builtin, special, or user defined function)
   --
   function isFunction(s : symb_index) return Boolean is
      t : constant symbol_type := symb_table(s).kind;
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL) or (t = SY_LAMBDA);
   end;
   --
   --  If symbol is a variable, get the symbol value.
   --
   function get_value(s : symb_index) return element_type is
   begin
      return symb_table(s).pv;
   end;
   --
   --  If symbol is a lambda, get the list.
   --
   function get_list(s : symb_index) return cons_index is
   begin
      return symb_table(s).ps;
   end;
   --
   --  Get a symbol's name
   --
   function get_name(s : symb_index) return string_index is
   begin
      return symb_table(s).str;
   end;
   --
   --  Get a symbol's reference count
   --
   function get_ref(s : symb_index) return Natural is
   begin
      return symb_table(s).ref;
   end;
   --
   --  Get a symbol from the symbol table
   --
   function get_sym(s : symb_index) return symbol is
   begin
      return symb_table(s);
   end;
   --
   --  Set a symbol entry
   --
   procedure set_sym(s : symb_index; val : symbol) is
   begin
      symb_table(s) := val;
   end;
   --
   --  Reset the symbol table
   --
   procedure reset_symbol_table is
   begin
      for i in symb_table'Range loop
         symb_table(i).ref := 0;
      end loop;
   end;
   --
end;
