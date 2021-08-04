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
   begin
      if s = NIL_CONS then
         error("peek8", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
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
         error("peek8", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      value := addr.all;
      e := (kind => V_INTEGER, i => int32(value));
   end;
   --
   procedure peek16(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint16;
      value : uint16;
   begin
      if s = NIL_CONS then
         error("peek16", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
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
         error("peek16", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      value := addr.all;
      e := (kind => V_INTEGER, i => int32(value));
   end;
   --
   procedure peek32(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint32;
      value : uint32;
   begin
      if s = NIL_CONS then
         error("peek32", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
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
         error("peek32", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then read the memory
      --  location and try to return the value.
      --
      value := addr.all;
      e := (kind => V_INTEGER, i => uint32_to_int32(value));
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
   begin
      if s = NIL_CONS then
         error("poke8", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Get the first parameter (address) and check if it is an integer
      --
      param := first_value(rest);
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint8(addr1);
      else
         error("poke8", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Get the second parameter (value)
      --
      if rest = NIL_CONS then
         error("poke8", "Only one parameter provided.");
         e := make_error(ERR_FEWPARAM);
         return;
      end if;
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := uint8(int32_to_uint32(param.i) and 16#FF#);
      else
         error("poke8", "Value must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      addr.all := value;
      e := (kind => V_INTEGER, i => int32(value));
   end;
   --
   procedure poke16(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint16;
      value : uint16;
   begin
      if s = NIL_CONS then
         error("poke16", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Get the first parameter (address) and check if it is an integer
      --
      param := first_value(rest);
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint16(addr1);
      else
         error("poke16", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Get the second parameter (value)
      --
      if rest = NIL_CONS then
         error("poke16", "Only one parameter provided.");
         e := make_error(ERR_FEWPARAM);
         return;
      end if;
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := uint16(int32_to_uint32(param.i) and 16#FFFF#);
      else
         error("poke16", "Value must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      addr.all := value;
      e := (kind => V_INTEGER, i => int32(value));
   end;
   --
   procedure poke32(e : out element_type; s : cons_index) is
      rest : cons_index := s;
      param : element_type;
      addr1 : intermediate;
      addr : p_uint32;
      value : uint32;
   begin
      if s = NIL_CONS then
         error("poke32", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Get the first parameter (address) and check if it is an integer
      --
      param := first_value(rest);
      if param.kind = V_INTEGER then
         addr1 := intermediate(int32_to_uint32(param.i));
         addr := intermediate_to_p_uint32(addr1);
      else
         error("poke32", "Address must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Get the second parameter (value)
      --
      if rest = NIL_CONS then
         error("poke32", "Only one parameter provided.");
         e := make_error(ERR_FEWPARAM);
         return;
      end if;
      param := first_value(rest);
      --
      --  Check if the first value is an integer element.
      --
      if param.kind = V_INTEGER then
         value := int32_to_uint32(param.i);
      else
         error("poke32", "Value must be integer.");
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      --
      --  If the parameter is an integer and in range, then write to the memory
      --  location.
      --
      addr.all := value;
      e := (kind => V_INTEGER, i => int32(value));
   end;
   --
end;
