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
      if not get_symb(sym_list, "LIST") then
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
   function coerce(s : cons_index) return element_type is
      s1 : cons_index := s;
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
      if s = cons_index'First then
         error("coerce", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      --
      --  Get first parameter.  Note that t1 and t2 are swapped around because in
      --  the first version of this function, the parameters were swapped.  This
      --  was not consistent with Common Lisp and has been changed.
      --
      if s1 > NIL_CONS then
         t2 := first_value(s1);
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
      t1 := first_value(s1);
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
   function concatenate(s : cons_index) return element_type is
      s1 : cons_index := s;
      t1 : element_type := NIL_ELEM;
      t2 : element_type := NIL_ELEM;
      v1 : value;
      v2 : value;
   begin
      if not_initialized then
         if not init_syms then
            error("concatenate", "Unable to initialize symbols");
            return(kind => E_ERROR);
         end if;
      end if;
      if s = cons_index'First then
         error("concatenate", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      --
      --  Get first parameter
      --
      t1 := first_value(s1);
      if t1.kind = E_ERROR then
         error("concatenate", "Error reported evaluating first parameter.");
         return t1;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("concatenate", "First parameter does not evaluate to a value.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if v1.kind /= V_QSYMBOL then
         error("concatenate", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         return (kind => E_ERROR);
      end if;
      if (v1.qsym /= sym_str) and (v1.qsym /= sym_list) then
         error("concatenate", "Can only concatinate strings and lists");
         put("Unrecognized type: ");
         print(v1);
         new_line;
      end if;
      --
      --  Second parameter - will probably move
      --
      --
      --  Process strings or lists
      --
      if v1.qsym = sym_str then
         --
         --  Concatinate strings
         --
         declare
            str_head : string_index := string_index'First;
            dest_str : string_index := string_index'First;
            src_str  : string_index := string_index'First;
            temp_str : string_index := string_index'First;
            dest_ptr : Integer;
            src_ptr  : integer;
         begin
            if not BBS.lisp.memory.alloc(str_head) then
               error("concatinate", "Unable to allocate string fragment.");
               BBS.lisp.memory.deref(t2);
               return (kind => E_ERROR);
            end if;
            dest_str := str_head;
            dest_ptr := 1;
            if s1 = NIL_CONS then
               error("concatenate", "Cannot compare a single element.");
               return (kind => E_ERROR);
            end if;
            while s1 > NIL_CONS loop
               t2 := first_value(s1);
               if t2.kind = E_ERROR then
                  error("concatenate", "Error reported evaluating second parameter.");
                  return t2;
               end if;
               if t2.kind = E_VALUE then
                  v2 := t2.v;
               else
                  error("concatenate", "Second parameter does not evaluate to a value");
                  BBS.lisp.memory.deref(t2);
                  return (kind => E_ERROR);
               end if;
               if v2.kind /= V_STRING then
                  error("concatinate", "Unable to concatinate " & value_type'Image(v2.kind) &
                          " to a string.");
                  BBS.lisp.memory.deref(v2);
                  return(kind => E_ERROR);
               end if;
               src_ptr := 1;
               src_str := v2.s;
               loop
                  string_table(dest_str).str(dest_ptr) := string_table(src_str).str(src_ptr);
                  string_table(dest_str).len := string_table(dest_str).len + 1;
                  dest_ptr := dest_ptr + 1;
                  src_ptr := src_ptr + 1;
                  if (src_ptr > fragment_len) or (src_ptr > string_table(src_str).len) then
                     src_str := string_table(src_str).next;
                     src_ptr := 1;
                  end if;
                  exit when src_str = string_index'First;
                  if dest_ptr > fragment_len then
                     if not BBS.lisp.memory.alloc(temp_str) then
                        error("concatinate", "Unable to allocate string fragment");
                        BBS.lisp.memory.deref(str_head);
                        BBS.lisp.memory.deref(t2);
                        return (kind => E_ERROR);
                     end if;
                     string_table(dest_str).next := temp_str;
                     dest_str := temp_str;
                     dest_ptr := 1;
                  end if;
               end loop;
               BBS.lisp.memory.deref(t2);
            end loop;
            return (kind => E_VALUE, v => (kind => V_STRING, s => str_head));
         end;
      elsif v1.qsym = sym_list then
         --
         -- Concatinate lists
         --
         declare
            cons_head : cons_index := cons_index'First;
            dest_cons : cons_index := cons_index'First;
            temp_cons : cons_index := cons_index'First;
            src_cons : cons_index := cons_index'First;
         begin
            if s1 = NIL_CONS then
               error("concatenate", "Cannot compare a single element.");
               return (kind => E_ERROR);
            end if;
            while s1 > NIL_CONS loop
                  t2 := first_value(s1);
               if t2.kind = E_ERROR then
                  error("concatenate", "Error reported evaluating second parameter.");
                  return t2;
               end if;
               if isList(t2) then
                  src_cons := getList(t2);
               else
                  error("concatenate", "Second parameter does not evaluate to a list");
                  BBS.lisp.memory.deref(t2);
                  return (kind => E_ERROR);
               end if;
               loop
                  if not BBS.lisp.memory.alloc(temp_cons) then
                     error("concatinate", "Unable to allocate cons.");
                     BBS.lisp.memory.deref(t2);
                     BBS.lisp.memory.deref(cons_head);
                     return (kind => E_ERROR);
                  end if;
                  if cons_head = cons_index'First then
                     cons_head := temp_cons;
                     dest_cons := temp_cons;
                  else
                     cons_table(dest_cons).cdr := (kind => E_VALUE, v => (kind => V_LIST, l => temp_cons));
                     dest_cons := temp_cons;
                  end if;
                  cons_table(dest_cons).car := cons_table(src_cons).car;
                  BBS.lisp.memory.ref(cons_table(dest_cons).car);
                  if isList(cons_table(src_cons).cdr) then
                     src_cons := getList(cons_table(src_cons).cdr);
                  else
                     exit;
                  end if;
               end loop;
               BBS.lisp.memory.deref(t2);
            end loop;
            return (kind => E_VALUE, v => (kind => V_LIST, l => cons_head));
         end;
      end if;
      --
      --  Get second parameter
      --
      --
      return (kind => E_ERROR);
   end;
   --
end;
