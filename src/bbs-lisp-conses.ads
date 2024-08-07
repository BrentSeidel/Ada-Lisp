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
--  This package contains definitions and routines relating to cons cells for the
--  tiny lisp interpreter.
--
package BBS.lisp.conses is
   pragma Elaborate_Body;
   --
   --  Types for cons reference counts
   --
   type cons_ref_count is new Natural;
   FREE_CONS : constant cons_ref_count := cons_ref_count'First;
   --
   --  A cons cell contains two element_type pointers that can point to either
   --  an atom or another cons cell.
   --
   type cons is
      record
         ref : cons_ref_count;
         car : element_type;
         cdr : element_type;
      end record;
   --
   --  Routines for accessing the cons table
   --
   function get_car(s : cons_index) return element_type
     with pre => (s > NIL_CONS);
   pragma Pure_Function(get_car);
   --
   procedure set_car(s : cons_index; e : element_type)
     with pre => (s > NIL_CONS);
   --
   function get_cdr(s : cons_index) return element_type
     with pre => (s > NIL_CONS);
   pragma Pure_Function(get_cdr);
   --
   procedure set_cdr(s : cons_index; e : element_type)
     with pre => (s > NIL_CONS);
   --
   function get_ref(s : cons_index) return cons_ref_count
     with pre => (s > NIL_CONS);
   pragma Pure_Function(get_ref);
   --  ------------------------------------------------------------------------
   --  Memory management
   --
   --  Ghost functions used in some proofs.
   --
   function count_free_cons return Natural
     with Ghost;
   --
   --  Reset the cons table
   --
   procedure reset_cons_table;
   --
   --  Allocate a cons cell.  The table is searched for an entry with
   --  a reference count of zero.  If such an entry is found, its reference
   --  count is set to 1 and the output parameter is set to the index of the
   --  entry and True is returned.  If no such value is found, False is returned
   --  and the output value should be ignored.
   --
   function alloc(s : out cons_index) return Boolean
     with post => (if count_free_cons'Old = 0 then alloc'Result = False
                else alloc'Result);
   --
   --  Increment the reference count a cons cell  This is typically done
   --  when an additional index to the item is created.
   --
   procedure ref(s : cons_index);
   --
   --  Decrement the reference count of a cons cell.  This is done when the
   --  reference is no longer needed.  If the reference count reaches 0, the
   --  item is considered to be deallocated.  In this case, if the item points
   --  to other items, they will be recursively dereffed.
   --
   procedure deref(s : cons_index);
private
   --
   --  The main data tables for various kinds of data.
   --
   --  Since this interpreter is designed to be used on embedded computers with
   --  no operating system and possibly no dynamic memory allocation, The
   --  statically allocated data structures are defined here.
   --
   cons_table : array (cons_index'First + 1 .. cons_index'Last) of cons;

end;
