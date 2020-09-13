with BBS.lisp.memory;
package body BBS.lisp.evaluate.symb is
   --
   --  Initialize the symbol indices.  Return true if successful or false it not.
   --
   function init_syms return Boolean is
   begin
      if not get_symb(sym_bool, "BOOLEAN") then
         return False;
      end if;
      if not get_symb(sym_char, "CHARACTER") then
         return False;
      end if;
      if not get_symb(sym_int, "INTEGER") then
         return False;
      end if;
      if not get_symb(sym_str, "STRING") then
         return False;
      end if;
      not_initialized := False;
      return True;
   end;
   --
   --  Coerces an object of one type to another type.  Available coercions are:
   --    character -> string
   --    boolean -> string
   --    boolean -> integer (NIL -> 0, T -> 1)
   --    integer -> boolean (0 -> NIL, /= 0 -> T)
   --
   --  (coerce <object> <result type>)
   --
   function coerce(e : element_type) return element_type is
      t  : element_type := e;
      t1 : element_type;
      t2 : element_type;
      v1 : value;
      v2 : value;
      str : string_index;
   begin
      if not_initialized then
         if not init_syms then
            error("coerce", "Unable to initialize symbols");
            return(kind => E_ERROR);
         end if;
      end if;
      if e.kind /= E_CONS then
         error("coerce", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      --
      --  Get first parameter.  Note that t1 and t2 are swapped around because in
      --  the first version of this function, the parameters were swapped.  This
      --  was not consistent with Common Lisp and has been changed.
      --
      if isList(t) then
         t2 := first_value(t);
      else
         error("coerce", "Cannot compare a single element.");
         return (kind => E_ERROR);
      end if;
      if t2.kind = E_ERROR then
         error("coerce", "Error reported evaluating second parameter.");
         return t2;
      end if;
      if t2.kind = E_VALUE then
         v2 := t2.v;
      else
         error("coerce", "Second parameter does not evaluate to a value");
         BBS.lisp.memory.deref(t2);
         return (kind => E_ERROR);
      end if;
      --
      --  Get second parameter
      --
      t1 := first_value(t);
      if t1.kind = E_ERROR then
         error("coerce", "Error reported evaluating first parameter.");
         return t1;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("coerce", "First parameter does not evaluate to a value.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if v1.kind /= V_QSYMBOL then
         error("coerce", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      --
      --  Now do the processing
      --
      if v1.qsym = sym_char then
         --  character -> character
         if v2.kind = V_CHARACTER then
            return (kind => E_VALUE, v => v2);
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to character type.");
            BBS.lisp.memory.deref(t2);
            return (kind => E_ERROR);
         end if;
      elsif v1.qsym = sym_int then
         if v2.kind = V_INTEGER then
            --  integer -> integer
            return (kind => E_VALUE, v => v2);
         elsif v2.kind = V_BOOLEAN then
            --  boolean -> integer (NIL -> 0, T -> 1)
            if v2.b then
               return (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            else
               return (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to integer type.");
            BBS.lisp.memory.deref(t2);
            return (kind => E_ERROR);
         end if;
      elsif v1.qsym = sym_bool then
         if v2.kind = V_BOOLEAN then
            -- boolean -> boolean
            return (kind => E_VALUE, v => v2);
         elsif v2.kind = V_INTEGER then
            --  integer -> boolean (0 -> NIL, /= 0 -> T)
            if v2.i = 0 then
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
            else
               return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to boolean type.");
            BBS.lisp.memory.deref(t2);
            return (kind => E_ERROR);
         end if;
      elsif v1.qsym = sym_str then
         if v2.kind = V_STRING then
            -- string -> string
            return (kind => E_VALUE, v => v2);
         elsif v2.kind = V_CHARACTER then
            --  character -> string
            if BBS.lisp.memory.alloc(str) then
               string_table(str).str(1) := v2.c;
               string_table(str).len := 1;
               return (kind => E_VALUE, v  => (kind => V_STRING, s => str));
            else
               error("coerce", "Unable to allocate string fragment.");
               return (kind => E_ERROR);
            end if;
         elsif v2.kind = V_BOOLEAN then
            --  boolean -> string
            if BBS.lisp.memory.alloc(str) then
               if v2.b then
                  string_table(str).str(1) := 'T';
                  string_table(str).len := 1;
               else
                  string_table(str).str(1) := 'N';
                  string_table(str).str(2) := 'I';
                  string_table(str).str(3) := 'L';
                  string_table(str).len := 3;
               end if;
               return (kind => E_VALUE, v  => (kind => V_STRING, s => str));
            else
               error("coerce", "Unable to allocate string fragment.");
               return (kind => E_ERROR);
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to string type.");
            BBS.lisp.memory.deref(t2);
            return (kind => E_ERROR);
         end if;
      else
         error("coerce", "Unknown coercion type.");
         put("Type is: ");
         print(v1);
         new_line;
         null;
      end if;
      BBS.lisp.memory.deref(t2);
      return (kind => E_ERROR);
   end;
   --
   function concatenate(e : element_type) return element_type is
      t  : element_type := e;
      t1 : element_type;
--      t2 : element_type;
      v1 : value;
--      v2 : value;
--      str : string_index;
   begin
      if not_initialized then
         if not init_syms then
            error("coerce", "Unable to initialize symbols");
            return(kind => E_ERROR);
         end if;
      end if;
      if e.kind /= E_CONS then
         error("coerce", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      --
      --  Get first parameter
      --
      t1 := first_value(t);
      if t1.kind = E_ERROR then
         error("coerce", "Error reported evaluating first parameter.");
         return t1;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("coerce", "First parameter does not evaluate to a value.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if v1.kind /= V_QSYMBOL then
         error("coerce", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      --
      --  Get second parameter
      --
      return (kind => E_ERROR);
   end;
   --
end;
