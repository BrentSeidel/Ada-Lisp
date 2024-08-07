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
with BBS.lisp.memory;
with BBS.lisp.parser;
with BBS.lisp.parser.string;
with BBS.lisp.strings;
package body BBS.lisp.evaluate.io is
   --
   --  Parser object used for read_expr.  This needs to be statically allocated
   --  so that the address can be taken.
   --
   str : aliased BBS.lisp.parser.string.parser_string;
   --
   --  Print stuff
   --
   procedure print(e : out element_type; s : cons_index) is
      t  : cons_index := s;
      car : element_type;
   begin
      while t > NIL_CONS loop
         car := first_value(t);
         print(car, True, False);
      end loop;
      e := NIL_ELEM;
   end;
   --
   --  Print an integer in hexidecimal format.  Sizes are byte, word, and long.
   --  Usage: (print-hex <number> <size>)
   --  size = 1 for byte
   --  size = 2 for word
   --  size = 3 or others for long
   --
   procedure print_hex(e : out element_type; s : cons_index) is
      t : cons_index := s;
      el : element_type;
      number : int32;
      size : int32 := 3;
   begin
      if s = NIL_CONS then
         error("print_hex", "No parameters provided.");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      el := first_value(t);
      if el.kind = V_INTEGER then
         number := el.i;
      else
         error("print_hex", "Can't process value of type " & value_type'Image(el.kind));
         BBS.lisp.memory.deref(el);
         e := make_error(ERR_WRONGTYPE);
         return;
      end if;
      el := first_value(t);
      if el.kind = V_INTEGER then
         if (el.i = 1) or (el.i = 2) then
            size := el.i;
         else
            size := 3;
         end if;
      else
         size := 3;
      end if;
      case size is
         when 1 =>  --  Byte
            put(toHexb(number));
         when 2 =>  --  Word
            put(toHexw(number));
         when others =>  --  Long
            put(toHexl(number));
      end case;
      e := NIL_ELEM;
   end;
   --
   procedure fresh_line(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      if not first_char_flag then
         New_Line;
      end if;
      e := NIL_ELEM;
   end;
   --
   procedure read_line(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
      buff : String(1 .. 256);
      size : Natural;
      str  : string_index;
      first : string_index;
   begin
      Get_Line(buff, size);
      if BBS.lisp.strings.alloc(str) then
         first := str;
         for ptr in buff'First .. size loop
            if not BBS.lisp.strings.append(str, buff(ptr)) then
               bbs.lisp.strings.deref(str);
               e := NIL_ELEM;
               return;
            end if;
         end loop;
      e := (kind => V_STRING, s => first);
      else
         e := NIL_ELEM;
      end if;
   end;
   --
   --  Read a line from a string and parse it.
   --
   procedure read_expr(e : out element_type; s : cons_index) is
      t   : cons_index := s;
      car : element_type;
   begin
      if s > NIL_CONS then
         car := first_value(t);
         if car.kind /= V_STRING then
            error("read_expr", "Must have a string parameter");
            e := make_error(ERR_WRONGTYPE);
            return;
         end if;
      else
         error("read_expr", "Must have a parameter");
         e := make_error(ERR_NOPARAM);
         return;
      end if;
      --
      --  Now we have a string parameter, parse it.
      --
      str.init(car.s);
      if not BBS.lisp.parser.parse(str'Access, e) then
         error("read_expr", "Parsing failed");
         BBS.lisp.memory.deref(e);
         e := make_error(ERR_PARSE);
      end if;
   end;
   --
   procedure terpri(e : out element_type; s : cons_index) is
      pragma Unreferenced (s);
   begin
      New_Line;
      e := NIL_ELEM;
   end;
   --
end;
