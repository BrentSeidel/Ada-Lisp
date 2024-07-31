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
with BBS.lisp.global;
with BBS.lisp.memory;
with BBS.lisp.symbols;
package body BBS.lisp.evaluate
with Refined_State =>  (pvt_exit_block => exit_block) is
   --
   --  Take an element_type and checks if it can be interpreted as true or false.
   --
   function isTrue(e : element_type) return Boolean is
   begin
      if e = NIL_ELEM then
         return False;
      elsif isList(e) then
         if (BBS.lisp.conses.get_car(getList(e)) = NIL_ELEM)
           and (BBS.lisp.conses.get_cdr(getList(e)) = NIL_ELEM) then
            return False;
         end if;
      elsif e.kind = V_BOOLEAN  then
         return e.b;
      end if;
      return True;
   end;
   --
   --  A list can be either element type E_CONS or a value of type V_LIST.  This
   --  checks both to see if the element is actually some sort of a list.
   --
   function isList(e : element_type) return Boolean is
   begin
      return e.kind = V_LIST;
   end;
   --
   --  If e is list type, return the index of the head of the list, otherwise
   --  return NIL_CONS.
   --
   function getList(e : element_type) return cons_index is
   begin
      if e.kind = V_LIST then
         return e.l;
      end if;
      return NIL_CONS;
   end;
   --
   --  Takes a cons index and returns a list element type.
   --
   function makeList(s : cons_index) return element_type is
   begin
      return (kind => V_LIST, l => s);
   end;
   --
   --  This checks to see if the element represents a function call.  The element
   --  is a symbol of type either BUILTIN or LAMBDA.
   --
   function isFunction(e : element_type) return Boolean is
      temp : element_type;
      list : cons_index;
      val  : element_type;
   begin
      list := getList(e);
      if list > NIL_CONS then
         temp := BBS.lisp.conses.get_car(list);
      else
         temp := e;
      end if;
      if temp.kind = V_SYMBOL then
         return BBS.lisp.symbols.isFunction(temp.sym);
      elsif temp.kind = V_LAMBDA then
         return True;
      elsif temp.kind = V_STACK then
         val := BBS.lisp.global.stack.search_frames(temp.st_offset, temp.st_name);
         if val.kind = V_LAMBDA then
            return True;
         end if;
         if val.kind = V_SYMBOL then
            return BBS.lisp.symbols.isFunction(val.sym);
         end if;
      end if;
      return False;
   end;
   --
   --  Evaluate a list of statements.
   --
   function execute_block(s : cons_index) return element_type is
      statement : cons_index := s;
      ret_val   : element_type;
   begin
      --
      --  Evaluate the function
      --
      ret_val := NIL_ELEM;
      while statement > NIL_CONS loop
         BBS.lisp.memory.deref(ret_val);
         if isList(BBS.lisp.conses.get_car(statement)) then
            ret_val := eval_dispatch(getList(BBS.lisp.conses.get_car(statement)));
         else
            ret_val := indirect_elem(BBS.lisp.conses.get_car(statement));
         end if;
         if ret_val.kind = V_ERROR then
            error("block execution", "Operation returned an error");
            exit;
         end if;
         if exit_block > 0 then
            exit;
         end if;
         statement := getList(BBS.lisp.conses.get_cdr(statement));
      end loop;
      return ret_val;
   end;
   --
   --  Set the exit_loop flag
   --
   procedure set_exit_block(n : Natural) is
   begin
      exit_block := n;
   end;
   --
   --  Decrement the exit_block flag
   --
   procedure decrement_exit_block is
   begin
      if exit_block > 0 then
         exit_block := exit_block - 1;
      end if;
   end;
   --
   --  Returns the exit_block flag
   --
   function get_exit_block return Natural is
   begin
      return exit_block;
   end;
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, it returns the atom that the variable points to.  If not, it
   --  just returns the atom.  If the variable points to a list, then the
   --  original atom is returned.
   --
   function indirect_elem(e : element_type) return element_type is
      sym : symbol_ptr;
      val : element_type;
   begin
      if e.kind = V_SYMBOL then
         sym := e.sym;
         if BBS.lisp.symbols.get_type(sym) = SY_VARIABLE then
            return BBS.lisp.symbols.get_value(sym);
         end if;
      end if;
      if e.kind = V_STACK then
         val := BBS.lisp.global.stack.search_frames(e.st_offset, e.st_name);
         return val;
      end if;
      return e;
   end;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   function first_value(s : in out cons_index) return element_type is
      first : element_type;
   begin
      if s = NIL_CONS then
         return NIL_ELEM;
      else
         first := BBS.lisp.conses.get_car(s);
         s := getList(BBS.lisp.conses.get_cdr(s));
         if first = NIL_ELEM then
            null;
         elsif isList(first) then
            if isFunction(first) then
               first := eval_dispatch(getList(first));
            else
               BBS.lisp.memory.ref(first);
            end if;
         else
            first := indirect_elem(first);
            BBS.lisp.memory.ref(first);
         end if;
      end if;
      return first;
   end;
end;
