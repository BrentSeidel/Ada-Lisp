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
   procedure peek8(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint8;
      value : uint8;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint8(addr1);
      else
         ok := False;
         error("peek8", "Address must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      if ok then
         value := addr.all;
         e := (kind => V_INTEGER, i => int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   procedure peek16(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint16;
      value : uint16;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint16(addr1);
      else
         ok := False;
         error("peek16", "Address must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      if ok then
         value := addr.all;
         e := (kind => V_INTEGER, i => int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   procedure peek32(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint32;
      value : uint32;
      ok : Boolean := True;
   begin
      --
      --  Get the first value
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint32(addr1);
      else
         ok := False;
         error("peek32", "Address must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      if ok then
         value := addr.all;
         e := (kind => V_INTEGER, i => uint32_to_int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   --  Write 8, 16, or 32 bit elements to memory locations.  Some systems may
   --  throw exceptions for unaligned access.
   --
   procedure poke8(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint8;
      value : uint8;
      ok : Boolean := True;
   begin
      --
      --  Get the first parameter (address)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint8(addr1);
      else
         ok := False;
         error("poke8", "Address must be integer.");
      end if;
      --
      --  Get the second parameter (value)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := uint8(int32_to_uint32(param.i) and 16#FF#);
      else
         ok := False;
         error("poke8", "Value must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      if ok then
         addr.all := value;
         e := (kind => V_INTEGER, i => int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   procedure poke16(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint16;
      value : uint16;
      ok : Boolean := True;
   begin
      --
      --  Get the first parameter (address)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint16(addr1);
      else
         ok := False;
         error("poke16", "Address must be integer.");
      end if;
      --
      --  Get the second parameter (value)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := uint16(int32_to_uint32(param.i) and 16#FFFF#);
      else
         ok := False;
         error("poke16", "Value must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      if ok then
         addr.all := value;
         e := (kind => V_INTEGER, i => int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --
   procedure poke32(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint32;
      value : uint32;
      ok : Boolean := True;
   begin
      --
      --  Get the first parameter (address)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint32(addr1);
      else
         ok := False;
         error("poke32", "Address must be integer.");
      end if;
      --
      --  Get the second parameter (value)
      --
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := int32_to_uint32(param.i);
      else
         ok := False;
         error("poke32", "Value must be integer.");
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      if ok then
         addr.all := value;
         e := (kind => V_INTEGER, i => int32(value));
      else
         e := make_error(ERR_UNKNOWN);
      end if;
   end;
   --

end;
