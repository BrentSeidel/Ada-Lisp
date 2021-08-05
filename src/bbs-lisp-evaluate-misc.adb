with Ada.Real_Time;
use type Ada.Real_Time.Time;
package body BBS.lisp.evaluate.misc is
   --
   --  Turn display of messages on or off depending on the boolean value
   --  passed as a parameter.
   --
   procedure msg(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if s = cons_index'First then
         error("msg", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("msg", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_BOOLEAN then
         msg_flag := p1.b;
         e := NIL_ELEM;
      else
         error("msg", "Parameter must be of boolean type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_WRONGTYPE);
      end if;
   end;
   --
   procedure dump(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      e := NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   procedure quit(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      exit_flag := True;
      e := NIL_ELEM;
   end;
   --
   --  Sleep for a specified period of time in mS.
   --
   procedure sleep(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
   begin
      if s = NIL_CONS then
         error("sleep", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      p1 := first_value(t);
      if p1.kind = V_ERROR then
         error("sleep", "Error reported evaluating parameter.");
         e := p1;
         return;
      end if;
      if p1.kind = V_INTEGER then
         delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span(Duration(p1.i)/1000.0);
         e := NIL_ELEM;
      else
         error("sleep", "Parameter must be of integer type, not " & value_type'Image(p1.kind));
         e := make_error(ERR_WRONGTYPE);
      end if;
   end;
   --
end;
