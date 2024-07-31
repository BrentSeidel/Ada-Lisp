--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of Tiny-Lisp.
--  Tiny-Lisp is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  Tiny-Lisp is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS.lisp.conses;
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
      str : string_index;
   begin
      if not_initialized then
         if not init_syms then
            error("coerce", "No parameters provided.");
            e := make_error(ERR_ALLOCSYM);
            return;
         end if;
      end if;
      if s = NIL_CONS then
         error("coerce", "Internal error.  Should have a list.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Get first parameter.  Note that t1 and t2 are swapped around because in
      --  the first version of this function, the parameters were swapped.  This
      --  was not consistent with Common Lisp and has been changed.
      --
      t2 := first_value(s1);
      if t2.kind = V_ERROR then
         error("coerce", "Error reported evaluating second parameter.");
         e := t2;
         return;
      end if;
      --
      --  Get second parameter
      --
      t1 := first_value(s1);
      if t1.kind = V_ERROR then
         error("coerce", "Error reported evaluating first parameter.");
         e := t1;
         return;
      end if;
      if t1.kind /= V_QSYMBOL then
         error("coerce", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Now do the processing
      --
      if t1.qsym = sym_char then
         --  character -> character
         if t2.kind = V_CHARACTER then
            e := t2;
         else
            error("coerce", "Unable to convert " & value_type'Image(t2.kind) &
                    " to character type.");
            BBS.lisp.memory.deref(t2);
            e := make_error(ERR_WRONGTYPE);
         end if;
         return;
      elsif t1.qsym = sym_int then
         if t2.kind = V_INTEGER then
            --  integer -> integer
            e := t2;
         elsif t2.kind = V_BOOLEAN then
            --  boolean -> integer (NIL -> 0, T -> 1)
            if t2.b then
               e := (kind => V_INTEGER, i => 1);
            else
               e := (kind => V_INTEGER, i => 0);
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(t2.kind) &
                    " to integer type.");
            BBS.lisp.memory.deref(t2);
            e := make_error(ERR_WRONGTYPE);
         end if;
         return;
      elsif t1.qsym = sym_bool then
         if t2.kind = V_BOOLEAN then
            -- boolean -> boolean
            e := t2;
         elsif t2.kind = V_INTEGER then
            --  integer -> boolean (0 -> NIL, /= 0 -> T)
            e := (kind => V_BOOLEAN, b => t2.i /= 0);
         else
            error("coerce", "Unable to convert " & value_type'Image(t2.kind) &
                    " to boolean type.");
            BBS.lisp.memory.deref(t2);
            e := make_error(ERR_WRONGTYPE);
         end if;
         return;
      elsif t1.qsym = sym_str then
         if t2.kind = V_STRING then
            -- string -> string
            e := t2;
         elsif t2.kind = V_CHARACTER then
            --  character -> string
            if BBS.lisp.strings.str_to_lisp(str, t2.c & "") then
               e := (kind => V_STRING, s => str);
            else
               error("coerce", "Unable to allocate string fragment.");
               e := make_error(ERR_ALLOCSTR);
            end if;
         elsif t2.kind = V_BOOLEAN then
            --  boolean -> string
            if t2.b then
               if BBS.lisp.strings.str_to_lisp(str, "T") then
                  e := (kind => V_STRING, s => str);
               else
                  error("coerce", "Unable to allocate string fragment.");
                  e := make_error(ERR_ALLOCSTR);
               end if;
            else
               if BBS.lisp.strings.str_to_lisp(str, "NIL") then
                  e := (kind => V_STRING, s => str);
               else
                  error("coerce", "Unable to allocate string fragment.");
                  e := make_error(ERR_ALLOCSTR);
               end if;
            end if;
         else
            error("coerce", "Unable to convert " & value_type'Image(t2.kind) &
                    " to string type.");
            BBS.lisp.memory.deref(t2);
            e := make_error(ERR_WRONGTYPE);
         end if;
         return;
      else
         error("coerce", "Unknown coercion type.");
         put("Type is: ");
         print(t1, False, True);
      end if;
      BBS.lisp.memory.deref(t2);
      e := make_error(ERR_WRONGTYPE);
   end;
   --
   procedure concatenate(e : out element_type; s : cons_index) is
      s1 : cons_index := s;
      t1 : element_type := NIL_ELEM;
      t2 : element_type := NIL_ELEM;
   begin
      if not_initialized then
         if not init_syms then
            error("concatenate", "Unable to initialize symbols");
            e := make_error(ERR_ALLOCSYM);
            return;
         end if;
      end if;
      if s = NIL_CONS then
         error("concatenate", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Get first parameter
      --
      t1 := first_value(s1);
      if t1.kind = V_ERROR then
         error("concatenate", "Error reported evaluating first parameter.");
         e := t1;
         return;
      end if;
      if t1.kind /= V_QSYMBOL then
         error("concatenate", "First parameter must be a quoted symbol.");
         BBS.lisp.memory.deref(t1);
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      if (t1.qsym /= sym_str) and (t1.qsym /= sym_list) then
         error("concatenate", "Can only concatenate strings and lists");
         put("Unrecognized type: ");
         print(t1, False, True);
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Process strings or lists
      --
      if t1.qsym = sym_str then
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
               e := make_error(ERR_ALLOCSTR);
               return;
            end if;
            dest_str := str_head;
            if s1 = NIL_CONS then
               error("concatenate", "Cannot concatenate a single element.");
               e := make_error(ERR_FEWPARAM);
               return;
            end if;
            while s1 > NIL_CONS loop
               --
               --  Second parameter - will probably move
               --
               t2 := first_value(s1);
               if t2.kind = V_ERROR then
                  error("concatenate", "Error reported evaluating second parameter.");
                  e := t2;
                  return;
               end if;
               if t2.kind /= V_STRING then
                  error("concatenate", "Unable to concatenate " & value_type'Image(t2.kind) &
                          " to a string.");
                  BBS.lisp.memory.deref(t2);
                  e := make_error(ERR_WRONGTYPE);
                  return;
               end if;
               if not BBS.lisp.strings.append(dest_str, t2.s) then
                  error("concatenate", "Unable to allocate string fragment");
                  BBS.lisp.strings.deref(str_head);
                  e := make_error(ERR_ALLOCSTR);
                  return;
               end if;
               BBS.lisp.memory.deref(t2);
            end loop;
            e := (kind => V_STRING, s => str_head);
            return;
         end;
      elsif t1.qsym = sym_list then
         --
         -- Concatinate lists
         --
         declare
            cons_head : cons_index := NIL_CONS;  --  Head of list being built
            dest_cons : cons_index := NIL_CONS;  --  Current element in list being built
            temp_cons : cons_index := NIL_CONS;  --  New cons cell to add to list
            src_cons : cons_index := NIL_CONS;   --  Source list to copy from
         begin
            if s1 = NIL_CONS then
               error("concatenate", "Cannot concatenate a single element.");
               e := make_error(ERR_FEWPARAM);
               return;
            end if;
            while s1 > NIL_CONS loop
               t2 := first_value(s1);
               if t2.kind = V_ERROR then
                  error("concatenate", "Error reported evaluating additional parameters.");
                  e := t2;
                  BBS.lisp.conses.deref(cons_head);
                  return;
               end if;
               src_cons := getList(t2);
               if src_cons = NIL_CONS then
                  error("concatenate", "Parameter does not evaluate to a list");
                  BBS.lisp.memory.deref(t2);
                  BBS.lisp.conses.deref(cons_head);
                  e := make_error(ERR_WRONGTYPE);
                  return;
               end if;
               loop
                  if not BBS.lisp.conses.alloc(temp_cons) then
                     error("concatenate", "Unable to allocate cons cell.");
                     BBS.lisp.conses.deref(cons_head);
                     BBS.lisp.conses.deref(src_cons);
                     e := make_error(ERR_ALLOCCONS);
                     return;
                  end if;
                  if cons_head = NIL_CONS then
                     cons_head := temp_cons;
                     dest_cons := temp_cons;
                  else
                     --
                     --  Point end of list to new cons cell and update end of list.
                     --
                     BBS.lisp.conses.set_cdr(dest_cons, (kind => V_LIST, l => temp_cons));
                     dest_cons := temp_cons;
                  end if;
                  --
                  --  Copy the value from the source list and move to the next
                  --  element in the source.
                  --
                  BBS.lisp.conses.set_car(dest_cons, BBS.lisp.conses.get_car(src_cons));
                  src_cons := getList(BBS.lisp.conses.get_cdr(src_cons));
                  if src_cons = NIL_CONS then
                     exit;
                  end if;
               end loop;
               BBS.lisp.memory.deref(t2);
            end loop;
            e := (kind => V_LIST, l => cons_head);
            return;
         end;
      end if;
   end;
   --
end;
