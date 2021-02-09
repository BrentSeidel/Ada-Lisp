package bbs.lisp.memory is
   --
   --  Utility functions for allocating and freeing items.  Memory management
   --  uses a reference counting system.  When references are added, the count
   --  should be incremented.  When references are deleted or go out of scope,
   --  the count should be decremented.
   --
   --  Symbols are allocated in another fashion and aren't deallocated, though
   --  this may change in the future.
   --
   --  Reset some of the memory tables back to their starting state.
   --
   procedure reset_tables
     with Global => (Output => (cons_table, symb_table, pvt_string_table));
   --
   --  Allocate various types of items.  The table is searched for an entry with
   --  a reference count of zero.  If such an entry is found, its reference
   --  count is set to 1 and the output parameter is set to the index of the
   --  entry and True is returned.  If no such value is found, False is returned
   --  and the output value should be ignored.
   --
   function alloc(s : out cons_index) return Boolean
     with Global => (Input => cons_table);
   -- should really be (In_Out => const_table);
   function alloc(s : out string_index) return Boolean
     with Global => (Input => pvt_string_table);
   -- should really be (In_Out => pvt_string_table);
   --
   --  Increment the reference count of various items.  This is typically done
   --  when an additional index to the item is created.
   --
   procedure ref(s : cons_index)
     with pre => (s > NIL_CONS),
     Global => (in_out => cons_table);
   procedure ref(s : string_index)
     with pre => (s > NIL_STR),
     Global => (In_Out => pvt_string_table);
   procedure ref(e : element_type)
     with Global => (In_Out => (cons_table, pvt_string_table));
   procedure ref(v : value)
     with Global => (In_Out => (cons_table, pvt_string_table));
   --
   --  Decrement the reference count for various items.  This is done when the
   --  reference is no longer needed.  If the reference count reaches 0, the
   --  item is considered to be deallocated.  In this case, if the item points
   --  to other items, they will be recursively dereffed.
   --
   procedure deref(s : cons_index)
     with pre => (s > NIL_CONS),
     Global => (in_out => cons_table);
   procedure deref(s : string_index)
     with pre => (s > NIL_STR),
     Global => (In_Out => pvt_string_table);
   procedure deref(e : element_type)
     with Global => (In_Out => (cons_table, pvt_string_table));
   procedure deref(v : value)
     with Global => (In_Out => (cons_table, pvt_string_table));
   --
end;
