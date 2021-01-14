--
--  This package contains an assortment of Lisp predicates that can be used to
--  get information about various objects.  Most of these will be very simple
--  and some of them will return constant values because Tiny-Lisp doesn't
--  implement some things.
--
package body BBS.lisp.evaluate.pred is
   --
   --  These return true of false depending on the type of data passed.
   --
   function atomp(s : cons_index) return element_type is
      p : element_type;
   begin
      if s = cons_index'First then
         error("symbolp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := cons_table(s).car;
      if not isList(p) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function characterp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("characterp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_CHARACTER) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function compiled_function_p(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("compiled_function_p", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if p.kind = E_SYMBOL then
         if (symb_table(p.sym).kind = SY_BUILTIN) or
           (symb_table(p.sym).kind = SY_SPECIAL) then
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
         end if;
      end if;
      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
   function consp(s : cons_index) return element_type is
      p : element_type;
   begin
      if s = cons_index'First then
         error("symbolp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := cons_table(s).car;
      if isList(p) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function functionp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("functionp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if p.kind = E_SYMBOL then
         if (symb_table(p.sym).kind = SY_BUILTIN) or
           (symb_table(p.sym).kind = SY_SPECIAL) or
           (symb_table(p.sym).kind = SY_LAMBDA) then
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
         end if;
      end if;
      if p.kind = E_VALUE then
         if p.v.kind = V_LAMBDA then
            return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
         end if;
      end if;
      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
   function integerp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("integerp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_INTEGER) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function listp(s : cons_index) return element_type is
      p : element_type;
   begin
      if s = cons_index'First then
         error("listp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := cons_table(s).car;
      if isList(p) or (p = NIL_ELEM) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function nullp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("nullp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if p = NIL_ELEM then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function numberp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("numberp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_INTEGER) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function simple_string_p(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("simple_string_p", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_STRING) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function stringp(s : cons_index) return element_type is
      t : element_type := (kind => E_CONS, ps => s);
      p : element_type;
   begin
      if s = cons_index'First then
         error("stringp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_STRING) then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   function symbolp(s : cons_index) return element_type is
      p : element_type;
   begin
      if s = cons_index'First then
         error("symbolp", "Internal error, not passed a list.");
         return (kind => E_ERROR);
      end if;
      p := cons_table(s).car;
      if p.kind = E_SYMBOL then
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   --  These always return false as the data types are not implemented.  There's
   --  actually no reason to have all of these functions coded.  They can just
   --  use one function that returns NIL (False).
   --
   function return_false(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
--   function rationalp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function floatp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function realp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function complexp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function bit_vector_p(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function vectorp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function simple_vector_p(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function simple_bit_vector_p(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function arrayp(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
--   function packagep(e : element_type) return element_type is
--      pragma Unreferenced (e);
--   begin
--      return (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
--   end;
   --
end;
