--
--  This package contains functions to support Lisp function definition and
--  evaluation.
--
package BBS.lisp.evaluate.func is
   --
   --  Defines a function.  The command is (defun name (parameters) body).
   --    name is a symbol of type LAMBDA.
   --    params is a list of the parameters for the function.  It must be a
   --      list of elements that translate to symbols or tempsyms.  Defun translates
   --      these to parameter elements.
   --    body is a list of the actions for the function.  This needs to be
   --      scanned and any symbol or tempsym that matches one of the params is
   --      translated to point to the parameter atom in the parameter list.  It
   --      also could concievable be a single atom or even NIL.
   --
   function defun(s : cons_index; p : phase) return element_type;
   --
   --  Defines a function.  The command is (lambda (parameters) body).
   --    params is a list of the parameters for the function.  It must be a
   --      list of elements that translate to symbols or tempsyms.  Defun translates
   --      these to parameter elements.
   --    body is a list of the actions for the function.  This needs to be
   --      scanned and any symbol or tempsym that matches one of the params is
   --      translated to point to the parameter atom in the parameter list.  It
   --      also could concievable be a single atom or even NIL.
   --    The returned value is an variable element of type V_LAMBDA.
   --
   function lambda(s : cons_index; p : phase) return element_type;
   --
   --  Functions for evaluating lisp functions
   --
   function eval_function(s : cons_index; e : element_type) return element_type;
end;
