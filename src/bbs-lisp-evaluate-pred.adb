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
--  This package contains an assortment of Lisp predicates that can be used to
--  get information about various objects.  Most of these will be very simple
--  and some of them will return constant values because Tiny-Lisp doesn't
--  implement some things.
--
with BBS.lisp.conses;
with BBS.lisp.memory;
with BBS.lisp.symbols;
package body BBS.lisp.evaluate.pred is
   --
   --  These return true of false depending on the type of data passed.
   --
   procedure atomp(e : out element_type; s : cons_index) is
   begin
      if s = NIL_CONS then
         error("atomp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      e := (Kind => V_BOOLEAN, b => not isList(BBS.lisp.conses.get_car(s)));
   end;
   --
   procedure characterp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("characterp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_CHARACTER);
   end;
   --
   procedure compiled_function_p(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("compiled_function_p", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      if p.kind = V_SYMBOL then
         if BBS.lisp.symbols.isFixed(p.sym) then
            e := ELEM_T;
            return;
         end if;
      end if;
      BBS.lisp.memory.deref(p);
      e := ELEM_F;
   end;
   --
   procedure consp(e : out element_type; s : cons_index) is
   begin
      if s = NIL_CONS then
         error("consp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      e := (Kind => V_BOOLEAN, b => isList(BBS.lisp.conses.get_car(s)));
   end;
   --
   procedure errorp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("errorp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_ERROR);
   end;
   --
   procedure functionp(e : out element_type; s : cons_index)is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("functionp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      if p.kind = V_SYMBOL then
         if BBS.lisp.symbols.isFunction(p.sym) then
            e := ELEM_T;
            BBS.lisp.memory.deref(p);
            return;
         end if;
      end if;
      if p.kind = V_LAMBDA then
         e := ELEM_T;
         BBS.lisp.memory.deref(p);
         return;
      end if;
      BBS.lisp.memory.deref(p);
      e := ELEM_F;
   end;
   --
   procedure integerp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("integerp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_INTEGER);
   end;
   --
   procedure listp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = NIL_CONS then
         error("listp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := BBS.lisp.conses.get_car(s);
      e := (Kind => V_BOOLEAN, b => isList(p) or (p = NIL_ELEM));
   end;
   --
   procedure nullp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("nullp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p = NIL_ELEM);
   end;
   --
   procedure numberp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("numberp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_INTEGER);
   end;
   --
   procedure simple_string_p(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("simple_string_p", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_STRING);
   end;
   --
   procedure stringp(e : out element_type; s : cons_index) is
      t : cons_index := s;
      p : element_type;
   begin
      if s = NIL_CONS then
         error("stringp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := first_value(t);
      e := (Kind => V_BOOLEAN, b => p.kind = V_STRING);
   end;
   --
   procedure symbolp(e : out element_type; s : cons_index) is
      p : element_type;
   begin
      if s = NIL_CONS then
         error("symbolp", "No parameter provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p := BBS.lisp.conses.get_car(s);
      e := (Kind => V_BOOLEAN, b => p.kind = V_SYMBOL);
   end;
   --
   --  These always return false as the data types are not implemented.  There's
   --  actually no reason to have all of these functions coded.  They can just
   --  use one function that returns NIL (False).
   --
   procedure return_false(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      e := ELEM_F;
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
