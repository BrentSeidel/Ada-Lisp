--
--  This package contains routines for direct memory access operations.  This
--  can is intended for use on embedded systems where hardware devices can be
--  accessed at fixed addresses.  Here be dragons.  This can be dangerous.
--
package BBS.lisp.evaluate.mem is
   --
   --  Read memory locations returning 8, 16, or 32 bit elements from the
   --  location.  Some systems may throw exceptions for unaligned access.
   --
   function peek8(e : element_type) return element_type;
   function peek16(e : element_type) return element_type;
   function peek32(e : element_type) return element_type;
   --
   --  Write 8, 16, or 32 bit elements to memory locations.  Some systems may
   --  throw exceptions for unaligned access.
   --
   function poke8(e : element_type) return element_type;
   function poke16(e : element_type) return element_type;
   function poke32(e : element_type) return element_type;
private
   type uint8 is mod 2**8
     with size => 8;
   type uint16 is mod 2**16
     with Size => 16;
   type intermediate is mod 2**(Standard'Address_Size)
     with Size => Standard'Address_Size;
   --
   --  uint32 is already defined.
   --
   type p_uint8 is access uint8;
   type p_uint16 is access uint16;
   type p_uint32 is access uint32;
   --
   --  On Arduino Due, reading memory locations:
   --  Addr  peek8   peek16     peek32
   --   1     D1      00D1     152000D1
   --   2     00      2000     DB152000
   --   3     20      1520     09DB1520
   --   4     15      DB15     0009DB15
   --   5     DB      09DB     5D0009DB
   --   6     09      0009     DB5D0009
   --   7     00      5D00     09DB5D00
   --   8     5D      DB5D     0009DB5D
end;
