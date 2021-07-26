with BBS.lisp.strings;
package body BBS.lisp.symbols is
   --
   --  Operations for symbols
   --
   --  Get the type of the symbol
   --
   function get_type(s : symb_index) return symbol_type is
   begin
      return symb_table(s).b.kind;
   end;
   --
   function get_type(s : symbol_ptr) return symbol_type is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b.kind;
      else
         return symb_table(s.d).b.kind;
      end if;
   end;
   --
   --  Check if symbol is fixed (builtin or special)
   --
   function isFixed(s : symb_index) return Boolean is
      t : constant symbol_type := symb_table(s).b.kind;
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL);
   end;
   --
   function isFixed(s : symbol_ptr) return Boolean is
      t : constant symbol_type := get_type(s);
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL);
   end;
   --
   --  Check if symbol is a function (builtin, special, or user defined function)
   --
   function isFunction(s : symb_index) return Boolean is
      t : constant symbol_type := symb_table(s).b.kind;
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL) or (t = SY_LAMBDA);
   end;
   --
   function isFunction(s : symbol_ptr) return Boolean is
      t : constant symbol_type := get_type(s);
   begin
      return (t = SY_BUILTIN) or (t = SY_SPECIAL) or (t = SY_LAMBDA);
   end;
   --
   --  If symbol is a variable, get the symbol value.
   --
   function get_value(s : symb_index) return element_type is
   begin
      return symb_table(s).b.pv;
   end;
   --
   function get_value(s : symbol_ptr) return element_type is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b.pv;
      else
         return symb_table(s.d).b.pv;
      end if;
   end;
   --
   --  If symbol is a lambda, get the list.
   --
   function get_list(s : symb_index) return cons_index is
   begin
      return symb_table(s).b.ps;
   end;
   --
   function get_list(s : symbol_ptr) return cons_index is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b.ps;
      else
         return symb_table(s.d).b.ps;
      end if;
   end;
   --
   --  Get a symbol's name
   --
   function get_name(s : symb_index) return string_index is
   begin
      return symb_table(s).name;
   end;
   --
   function get_name(s : symbol_ptr) return string_index is
   begin
      return symb_table(s.d).name;
   end;
   --
   function get_name(s : symbol_ptr) return access constant String is
   begin
      return index(s.f).name;
   end;
   --
   --  Get a symbol's reference count
   --
   function get_ref(s : symb_index) return Natural is
   begin
      return symb_table(s).ref;
   end;
   --
   function get_ref(s : symbol_ptr) return Natural is
   begin
      if s.kind = ST_FIXED then
         return 2;
      else
         return symb_table(s.d).ref;
      end if;
   end;
   --
   --  Get a symbol from the symbol table
   --
   function get_sym(s : symb_index) return symbol is
   begin
      return symb_table(s);
   end;
   --
   function get_sym(s : symbol_ptr) return sym_body is
   begin
      if s.kind = ST_FIXED then
         return index(s.f).b;
      else
         return symb_table(s.d).b;
      end if;
   end;
   --
   --  Set a symbol entry
   --
   procedure set_sym(s : symb_index; val : symbol) is
   begin
      symb_table(s) := val;
   end;
   --
   procedure set_sym(s : symbol_ptr; val : sym_body) is
   begin
      symb_table(s.d).b := val;
--      case val.Kind is
--         when SY_SPECIAL =>
--            symb_table(s.d) := (ref => 1, name => get_name(s),
--                                kind => SY_SPECIAL, b => val);
--         when SY_BUILTIN =>
--            symb_table(s.d) := (ref => 1, name => get_name(s),
--                                kind => SY_BUILTIN, b => val);
--         when SY_LAMBDA =>
--            symb_table(s.d) := (ref => 1, name => get_name(s),
--                                kind => SY_LAMBDA, b => val);
--         when SY_VARIABLE =>
--            symb_table(s.d) := (ref => 1, name => get_name(s),
--                                kind => SY_VARIABLE, b => val);
--         when SY_EMPTY =>
--            symb_table(s.d) := (ref => 1, name => get_name(s),
--                                kind => SY_EMPTY, b => val);
--      end case;
   end;
   --
   --  Add a new symbol entry
   --
   procedure add_sym(s : symbol_ptr; val : symbol) is
   begin
      symb_table(s.d) := val;
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
   --  Search the symbol table for a name.  Assume that the string is already
   --  uppercased.
   --
   function find_name(s : string_index) return symbol_ptr is
   begin
      for i in index'Range loop
         if bbs.lisp.strings.compare(s, index(i).name.all) = CMP_EQ then
            return (kind => ST_FIXED, f => i);
         end if;
      end loop;
      return (kind => ST_NULL);
   end;
   --
end;
