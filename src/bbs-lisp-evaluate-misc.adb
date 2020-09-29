with Ada.Real_Time;
use type Ada.Real_Time.Time;
package body BBS.lisp.evaluate.misc is
   --
   function reset(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      init;
      return NIL_ELEM;
   end;
   --
   --  Turn display of messages on or off depending on the boolean value
   --  passed as a parameter.
   --
   function msg(e : element_type) return element_type is
      t : element_type := e;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if t.kind /= E_CONS then
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
   function dump(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      return NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      exit_flag := True;
      return NIL_ELEM;
   end;
   --
   --  Sleep for a specified period of time in mS.
   --
   function sleep(e : element_type) return element_type is
      t : element_type := e;
      p1 : element_type; --  Parameter
      v : value;
   begin
      if e.kind /= E_CONS then
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
