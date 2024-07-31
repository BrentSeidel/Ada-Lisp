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
   procedure defun(e : out element_type; s : cons_index; p : phase);
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
   procedure lambda(e : out element_type; s : cons_index; p : phase);
   --
   --  Evaluates a lisp form.  Basically this takes the first parameter and passes
   --  it to the evaluator.
   --
   procedure eval_list(e : out element_type; s : cons_index);
   --
   --  Functions for evaluating lisp functions
   --
   function eval_function(s : cons_index; e : cons_index) return element_type;
end;
