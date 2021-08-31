--
--  This package contains operations that use either boolean types or bits of
--  integers.
--
package BBS.lisp.evaluate.bool is
   pragma Elaborate_Body;
   --
   --  Perform a logical NOT operation.
   --
   procedure eval_not(e : out element_type; s : cons_index);
   --
   --  Perform a logical AND operation.
   --
   procedure eval_and(e : out element_type; s : cons_index);
   --
   --  Perform a logical OR operation.
   --
   procedure eval_or(e : out element_type; s : cons_index);
end;
