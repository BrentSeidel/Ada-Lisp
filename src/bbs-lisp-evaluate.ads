--
--  This package contains helper functions for evaluating the various Lisp
--  operatations.  The actual operations are in subpackages of this one.
--
package BBS.lisp.evaluate is
   --
   function isTrue(e : element_type) return Boolean
     with Global => (Input => cons_table);
   function isList(e : element_type) return Boolean
     with Global => Null;
   function isFunction(e : element_type) return Boolean
     with Global => (Input => cons_table);
   function getList(e : element_type) return cons_index
     with post => (if not isList(e) then getList'Result = NIL_CONS else
                       getList'Result in cons_index'Range),
       global => Null;
   --
   --  Execute the statements in a block and return the value of the last
   --  statement executed.
   --
   function execute_block(e : element_type) return element_type;
   --
   --  The following function examines an atom.  If the atom is some sort of
   --  variable, an element type pointing to the value.  If not, the element
   --  points to the original atom.
   --
   function indirect_elem(e : element_type) return element_type;
   --
   --  This procedure extracts the first value from an element.  This value may
   --  be a value, a variable, or a list.  If the list starts with an expression,
   --  it is passed to the evaluator and the results returned.  The rest of the
   --  expression is also returned
   --
   function first_value(s : in out cons_index) return element_type;
   --
end;
