--
--  This package contains an assortment of Lisp predicates that can be used to
--  get information about various objects.  Most of these will be very simple
--  and some of them will return constant values because Tiny-Lisp doesn't
--  implement some things.
--
with BBS.lisp.memory;
with BBS.lisp.symbols;
package body BBS.lisp.evaluate.pred is
   --
   --  These return true of false depending on the type of data passed.
   --
   procedure atomp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = cons_index'First then
         error("atomp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := cons_table(s).car;
      if not isList(p) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure characterp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("characterp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_CHARACTER) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure compiled_function_p(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("compiled_function_p", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if p.kind = E_SYMBOL then
         if BBS.lisp.symbols.isFixed(p.sym) then
            e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
            return;
         end if;
      end if;
      BBS.lisp.memory.deref(p);
      e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
   procedure consp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = NIL_CONS then
         error("consp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := cons_table(s).car;
      if isList(p) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure errorp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("errorp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_ERROR) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure functionp(e : out element_type; s : cons_index)is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("functionp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if p.kind = E_SYMBOL then
         if BBS.lisp.symbols.isFunction(p.sym) then
            e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
            BBS.lisp.memory.deref(p);
            return;
         end if;
      end if;
      if p.kind = E_VALUE then
         if p.v.kind = V_LAMBDA then
            e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
            BBS.lisp.memory.deref(p);
            return;
         end if;
      end if;
      BBS.lisp.memory.deref(p);
      e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
   procedure integerp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("integerp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_INTEGER) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure listp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = NIL_CONS then
         error("listp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := cons_table(s).car;
      if isList(p) or (p = NIL_ELEM) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure nullp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("nullp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if p = NIL_ELEM then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure numberp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("numberp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_INTEGER) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure simple_string_p(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("simple_string_p", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_STRING) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure stringp(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("stringp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := first_value(t);
      if (p.kind = E_VALUE) and then (p.v.kind = V_STRING) then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   procedure symbolp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = NIL_CONS then
         error("symbolp", "Internal error, not passed a list.");
         e := make_error(ERR_UNKNOWN);
         return;
      end if;
      p := cons_table(s).car;
      if p.kind = E_SYMBOL then
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True));
      else
         e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
      end if;
   end;
   --
   --  These always return false as the data types are not implemented.  There's
   --  actually no reason to have all of these functions coded.  They can just
   --  use one function that returns NIL (False).
   --
   procedure return_false(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      e := (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False));
   end;
   --
--   procedure arrayp(e : out element_type; s : cons_index);
--   procedure bit_vector_p(e : out element_type; s : cons_index);
--   procedure complexp(e : out element_type; s : cons_index);
--   procedure floatp(e : out element_type; s : cons_index);
--   procedure rationalp(e : out element_type; s : cons_index);
--   procedure realp(e : out element_type; s : cons_index);
--   procedure packagep(e : out element_type; s : cons_index);
--   procedure simple_vector_p(e : out element_type; s : cons_index);
--   procedure simple_bit_vector_p(e : out element_type; s : cons_index);
--   procedure vectorp(e : out element_type; s : cons_index);
   --
end;
