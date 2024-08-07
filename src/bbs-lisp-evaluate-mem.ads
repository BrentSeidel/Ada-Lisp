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
--  This package contains routines for direct memory access operations.  This
--  can is intended for use on embedded systems where hardware devices can be
--  accessed at fixed addresses.  Here be dragons.  This can be dangerous.
--
package BBS.lisp.evaluate.mem
  with SPARK_Mode => Off
is
   pragma Elaborate_Body;
   --
   --  Since this package explicitly access arbitrary memory locations for read
   --  and write, there is no way that this can be proved, so we just turn SPARK
   --  mode off.
   --
   --  Read memory locations returning 8, 16, or 32 bit elements from the
   --  location.  Some systems may throw exceptions for unaligned access.
   --
   procedure peek8(e : out element_type; s : cons_index);
   procedure peek16(e : out element_type; s : cons_index);
   procedure peek32(e : out element_type; s : cons_index);
   --
   --  Write 8, 16, or 32 bit elements to memory locations.  Some systems may
   --  throw exceptions for unaligned access.
   --
   procedure poke8(e : out element_type; s : cons_index);
   procedure poke16(e : out element_type; s : cons_index);
   procedure poke32(e : out element_type; s : cons_index);
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
