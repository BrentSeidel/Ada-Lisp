with BBS.lisp.utilities;
with Ada.Unchecked_Conversion;
package body BBS.lisp.evaluate.mem is
   --
   -- Conversions to addresses
   --
   function intermediate_to_p_uint8 is
      new Ada.Unchecked_Conversion(source => intermediate, target => p_uint8);
   function intermediate_to_p_uint16 is
      new Ada.Unchecked_Conversion(source => intermediate, target => p_uint16);
   function intermediate_to_p_uint32 is
      new Ada.Unchecked_Conversion(source => intermediate, target => p_uint32);
   --
   --  Read memory locations returning 8, 16, or 32 bit elements from the
   --  location.  Some systems may throw exceptions for unaligned access.
   --
   function peek8(e : element_type) return element_type is
      param : BBS.lisp.element_type;
      addr1 : intermediate;
      addr : p_uint8;
      rest : BBS.lisp.element_type;
      el : BBS.lisp.element_type;
      value : uint8;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      BBS.lisp.utilities.first_value(e, param, rest);
      --
      --  Check if the first value is an integer atom.
      --
      if param.kind = BBS.lisp.E_VALUE then
         if param.v.kind = BBS.lisp.V_INTEGER then
            addr1 := intermediate(int32_to_uint32(param.v.i));
            addr := intermediate_to_p_uint8(addr1);
         else
            ok := False;
            BBS.lisp.error("peek8", "Parameter must be integer.");
         end if;
      else
         ok := False;
         BBS.lisp.error("peek8", "Parameter must be an element.");
         BBS.lisp.print(param, False, True);
      end if;
      --
      --  If the parameter is an integer and in range, then read the pin and try
      --  to return the value.
      --
      if ok then
         value := addr.all;
         el := (kind => BBS.lisp.E_VALUE, v => (kind => BBS.lisp.V_INTEGER, i => int32(value)));
      else
         el := (kind => E_ERROR);
      end if;
      return el;
   end;
   --
   function peek16(e : element_type) return element_type is
      param : BBS.lisp.element_type;
      addr1 : intermediate;
      addr : p_uint16;
      rest : BBS.lisp.element_type;
      el : BBS.lisp.element_type;
      value : uint16;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      BBS.lisp.utilities.first_value(e, param, rest);
      --
      --  Check if the first value is an integer atom.
      --
      if param.kind = BBS.lisp.E_VALUE then
         if param.v.kind = BBS.lisp.V_INTEGER then
            addr1 := intermediate(int32_to_uint32(param.v.i));
            addr := intermediate_to_p_uint16(addr1);
         else
            ok := False;
            BBS.lisp.error("peek16", "Parameter must be integer.");
         end if;
      else
         ok := False;
         BBS.lisp.error("peek16", "Parameter must be an element.");
         print(param, False, True);
         el := (kind => E_ERROR);
      end if;
      --
      --  If the parameter is an integer and in range, then read the pin and try
      --  to return the value.
      --
      if ok then
         value := addr.all;
         el := (kind => BBS.lisp.E_VALUE, v => (kind => BBS.lisp.V_INTEGER, i => int32(value)));
      else
         el := (kind => E_ERROR);
      end if;
      return el;
   end;
   --
   function peek32(e : element_type) return element_type is
      param : BBS.lisp.element_type;
      addr1 : intermediate;
      addr : p_uint32;
      rest : BBS.lisp.element_type;
      el : BBS.lisp.element_type;
      value : uint32;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      BBS.lisp.utilities.first_value(e, param, rest);
      --
      --  Check if the first value is an integer atom.
      --
      if param.kind = BBS.lisp.E_VALUE then
         if param.v.kind = BBS.lisp.V_INTEGER then
            addr1 := intermediate(int32_to_uint32(param.v.i));
            addr := intermediate_to_p_uint32(addr1);
         else
            ok := False;
            BBS.lisp.error("peek32", "Parameter must be integer.");
         end if;
      else
         ok := False;
         BBS.lisp.error("peek32", "Parameter must be an element.");
         print(param, False, True);
         el := (kind => E_ERROR);
      end if;
      --
      --  If the parameter is an integer and in range, then read the pin and try
      --  to return the value.
      --
      if ok then
         value := addr.all;
         el := (kind => BBS.lisp.E_VALUE, v => (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(value)));
      else
         el := (kind => E_ERROR);
      end if;
      return el;
   end;
   --
   --  Write 8, 16, or 32 bit elements to memory locations.  Some systems may
   --  throw exceptions for unaligned access.
   --
   function poke8(e : element_type) return element_type is
   begin
      return (kind => E_ERROR);
   end;
   --
   function poke16(e : element_type) return element_type is
   begin
      return (kind => E_ERROR);
   end;
   --
   function poke32(e : element_type) return element_type is
   begin
      return (kind => E_ERROR);
   end;
   --

end;
