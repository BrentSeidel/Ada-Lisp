--
--  This package contains all the global data structures for the Lisp interpreter.
--
with BBS.lisp.stack;
package BBS.lisp.global is
   stack : BBS.lisp.stack.lisp_stack(max_stack);
end;
