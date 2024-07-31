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
--  This package manages allocations in the cons and string arrays.  These can
--  be dynamically allocated and deallocated.  Reference counting is used to
--  determine if a particular entry is free or not.
--
package BBS.lisp.memory is
   --
   --  Utility functions for allocating and freeing items.  Memory management
   --  uses a reference counting system.  When references are added, the count
   --  should be incremented.  When references are deleted or go out of scope,
   --  the count should be decremented.
   --
   --  Since the tables have been moved into separate packages, this package is
   --  more of a dispatcher for elements.
   --
   --  Reset some of the memory tables back to their starting state.
   --
   procedure reset_tables;
   --
   --  Increment the reference count of various items.  This is typically done
   --  when an additional index to the item is created.
   --
   procedure ref(e : element_type);
   --
   --  Decrement the reference count for various items.  This is done when the
   --  reference is no longer needed.  If the reference count reaches 0, the
   --  item is considered to be deallocated.  In this case, if the item points
   --  to other items, they will be recursively dereffed.
   --
   procedure deref(e : element_type);
   --
end;
