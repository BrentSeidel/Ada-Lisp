with BBS.lisp.memory;
with BBS.lisp.strings;
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
   procedure coerce(e : out element_type; s : cons_index) is
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
            e :=(kind => E_ERROR);
            return;
         end if;
      end if;
      if s = NIL_CONS then
         error("coerce", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Get first parameter.  Note that t1 and t2 are swapped around because in
      --  the first version of this function, the parameters were swapped.  This
      --  was not consistent with Common Lisp and has been changed.
      --
      t2 := first_value(s1);
      if t2.kind = E_ERROR then
         error("coerce", "Error reported evaluating second parameter.");
         e := t2;
         return;
      end if;
      if t2.kind = E_VALUE then
         v2 := t2.v;
      else
         error("coerce", "Second parameter does not evaluate to a value");
         BBS.lisp.memory.deref(t2);
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Get second parameter
      --
      t1 := first_value(s1);
      if t1.kind = E_ERROR then
         error("coerce", "Error reported evaluating first parameter.");
         e := t1;
         return;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("coerce", "First parameter does not evaluate to a value.");
         BBS.lisp.memory.deref(t1);
         e := (kind => E_ERROR);
         return;
      end if;
      if v1.kind /= V_QSYMBOL then
         error("coerce", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Now do the processing
      --
      if v1.qsym = sym_char then
         --  character -> character
         if v2.kind = V_CHARACTER then
            e := (kind => E_VALUE, v => v2);
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to character type.");
            BBS.lisp.memory.deref(t2);
            e := (kind => E_ERROR);
         end if;
         return;
      elsif v1.qsym = sym_int then
         if v2.kind = V_INTEGER then
            --  integer -> integer
            e := (kind => E_VALUE, v => v2);
         elsif v2.kind = V_BOOLEAN then
            --  boolean -> integer (NIL -> 0, T -> 1)
            if v2.b then
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 1));
            else
               e := (kind => E_VALUE, v => (kind => V_INTEGER, i => 0));
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to integer type.");
            BBS.lisp.memory.deref(t2);
            e := (kind => E_ERROR);
         end if;
         return;
      elsif v1.qsym = sym_bool then
         if v2.kind = V_BOOLEAN then
            -- boolean -> boolean
            e := (kind => E_VALUE, v => v2);
         elsif v2.kind = V_INTEGER then
            --  integer -> boolean (0 -> NIL, /= 0 -> T)
            if v2.i = 0 then
               e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
            else
               e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to boolean type.");
            BBS.lisp.memory.deref(t2);
            e := (kind => E_ERROR);
         end if;
         return;
      elsif v1.qsym = sym_str then
         if v2.kind = V_STRING then
            -- string -> string
            e := (kind => E_VALUE, v => v2);
         elsif v2.kind = V_CHARACTER then
            --  character -> string
            if BBS.lisp.strings.str_to_lisp(str, v2.c & "") then
               e := (kind => E_VALUE, v  => (kind => V_STRING, s => str));
            else
               error("coerce", "Unable to allocate string fragment.");
               e := (kind => E_ERROR);
            end if;
         elsif v2.kind = V_BOOLEAN then
            --  boolean -> string
            if v2.b then
               if BBS.lisp.strings.str_to_lisp(str, "T") then
                  e := (kind => E_VALUE, v  => (kind => V_STRING, s => str));
               else
                  error("coerce", "Unable to allocate string fragment.");
                  e := (kind => E_ERROR);
               end if;
            else
               if BBS.lisp.strings.str_to_lisp(str, "NIL") then
                  e := (kind => E_VALUE, v  => (kind => V_STRING, s => str));
               else
                  error("coerce", "Unable to allocate string fragment.");
                  e := (kind => E_ERROR);
               end if;
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(v2.kind) &
                    " to string type.");
            BBS.lisp.memory.deref(t2);
            e := (kind => E_ERROR);
         end if;
         return;
      else
         error("coerce", "Unknown coercion type.");
         put("Type is: ");
         print(v1);
         new_line;

      end if;
      BBS.lisp.memory.deref(t2);
      e := (kind => E_ERROR);
   end;
   --
   procedure concatenate(e : out element_type; s : cons_index) is
      s1 : cons_index := s;
      t1 : element_type := NIL_ELEM;
      t2 : element_type := NIL_ELEM;
      v1 : value;
      v2 : value;
   begin
      if not_initialized then
         if not init_syms then
            error("concatenate", "Unable to initialize symbols");
            e :=(kind => E_ERROR);
            return;
         end if;
      end if;
      if s = NIL_CONS then
         error("concatenate", "Internal error.  Should have a list.");
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Get first parameter
      --
      t1 := first_value(s1);
      if t1.kind = E_ERROR then
         error("concatenate", "Error reported evaluating first parameter.");
         e := t1;
         return;
      end if;
      if t1.kind = E_VALUE then
         v1 := t1.v;
      else
         error("concatenate", "First parameter does not evaluate to a value.");
         BBS.lisp.memory.deref(t1);
         e := (kind => E_ERROR);
         return;
      end if;
      if v1.kind /= V_QSYMBOL then
         error("concatenate", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         e := (kind => E_ERROR);
         return;
      end if;
      if (v1.qsym /= sym_str) and (v1.qsym /= sym_list) then
         error("concatenate", "Can only concatenate strings and lists");
         put("Unrecognized type: ");
         print(v1);
         new_line;
         e := (kind => E_ERROR);
         return;
      end if;
      --
      --  Process strings or lists
      --
      if v1.qsym = sym_str then
         --
         --  Concatinate strings
         --
         declare
            str_head : string_index := NIL_STR;
            dest_str : string_index := NIL_STR;
         begin
            if not BBS.lisp.strings.alloc(str_head) then
               error("concatenate", "Unable to allocate string fragment.");
               BBS.lisp.memory.deref(t2);
               e := (kind => E_ERROR);
               return;
            end if;
            dest_str := str_head;
            if s1 = NIL_CONS then
               error("concatenate", "Cannot concatenate a single element.");
               e := (kind => E_ERROR);
               return;
            end if;
            while s1 > NIL_CONS loop
               --
               --  Second parameter - will probably move
               --
               t2 := first_value(s1);
               if t2.kind = E_ERROR then
                  error("concatenate", "Error reported evaluating second parameter.");
                  e := t2;
                  return;
               end if;
               if t2.kind = E_VALUE then
                  v2 := t2.v;
               else
                  error("concatenate", "Parameter does not evaluate to a value");
                  BBS.lisp.memory.deref(t2);
                  e := (kind => E_ERROR);
                  return;
               end if;
               if v2.kind /= V_STRING then
                  error("concatenate", "Unable to concatenate " & value_type'Image(v2.kind) &
                          " to a string.");
                  BBS.lisp.memory.deref(t2);
                  e := (kind => E_ERROR);
                  return;
               end if;
               if not BBS.lisp.strings.append(dest_str, v2.s) then
                  error("concatenate", "Unable to allocate string fragment");
                  BBS.lisp.strings.deref(str_head);
                  e := (kind => E_ERROR);
                  return;
               end if;
               BBS.lisp.memory.deref(t2);
            end loop;
            e := (kind => E_VALUE, v => (kind => V_STRING, s => str_head));
            return;
         end;
      elsif v1.qsym = sym_list then
         --
         -- Concatinate lists
         --
         declare
            cons_head : cons_index := NIL_CONS;
            dest_cons : cons_index := NIL_CONS;
            temp_cons : cons_index := NIL_CONS;
            src_cons : cons_index := NIL_CONS;
         begin
            if s1 = NIL_CONS then
               error("concatenate", "Cannot concatenate a single element.");
               e := (kind => E_ERROR);
               return;
            end if;
            while s1 > NIL_CONS loop
               t2 := first_value(s1);
               if t2.kind = E_ERROR then
                  error("concatenate", "Error reported evaluating additional parameters.");
                  e := t2;
                  return;
               end if;
               src_cons := getList(t2);
               if src_cons = NIL_CONS then
                  error("concatenate", "Parameter does not evaluate to a list");
                  BBS.lisp.memory.deref(t2);
                  e := (kind => E_ERROR);
                  return;
               end if;
               loop
                  if not BBS.lisp.memory.alloc(temp_cons) then
                     error("concatenate", "Unable to allocate cons cell.");
                     BBS.lisp.memory.deref(cons_head);
                     e := (kind => E_ERROR);
                     return;
                  end if;
                  if cons_head = NIL_CONS then
                     cons_head := temp_cons;
                     dest_cons := temp_cons;
                  else
                     cons_table(dest_cons).cdr := (kind => E_VALUE, v => (kind => V_LIST, l => temp_cons));
                     dest_cons := temp_cons;
                  end if;
                  cons_table(dest_cons).car := cons_table(src_cons).car;
                  BBS.lisp.memory.ref(cons_table(dest_cons).car);
                  src_cons := getList(cons_table(src_cons).cdr);
                  if src_cons = NIL_CONS then
                     exit;
                  end if;
               end loop;
               BBS.lisp.memory.deref(t2);
            end loop;
            e := (kind => E_VALUE, v => (kind => V_LIST, l => cons_head));
            return;
         end;
      end if;
   end;
   --
end;
