with Ada.Real_Time;
use type Ada.Real_Time.Time;
package body BBS.lisp.evaluate.misc is
   --
   --  Turn display of messages on or off depending on the boolean value
   --  passed as a parameter.
   --
   function msg(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if s = cons_index'First then
         error("msg", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("msg", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("msg", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_BOOLEAN then
         msg_flag := v.b;
         return NIL_ELEM;
      else
         error("msg", "Parameter must be of boolean type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
   function dump(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      return NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(s : cons_index) return element_type is
      pragma Unreferenced (s);
   begin
      exit_flag := True;
      return NIL_ELEM;
   end;
   --
   --  Sleep for a specified period of time in mS.
   --
   function sleep(s : cons_index) return element_type is
      t  : cons_index := s;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if s = NIL_CONS then
         error("sleep", "Internal error.  Should have a list.");
         return (kind => E_ERROR);
      end if;
      p1 := first_value(t);
      if p1.kind = E_ERROR then
         error("sleep", "Error reported evaluating parameter.");
         return p1;
      end if;
      if p1.kind = E_VALUE then
         v := p1.v;
      else
         error("sleep", "Parameter does not evaluate to a value");
      end if;
      if v.kind = V_INTEGER then
         delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span(Duration(v.i)/1000.0);
         return NIL_ELEM;
      else
         error("sleep", "Parameter must be of integer type, not " & value_type'Image(v.kind));
         return (kind => E_ERROR);
      end if;
   end;
   --
end;
