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
--  with Tiny-Lisp. If not, see <https://www.gnu.org/licenses/>.
--
--  This package handles the parsing of input.  Input can be a list, an atom
--  of either a symbol or integer, or a comment.
--
package BBS.lisp.parser is
   --
   --  Define an abstract class for providing data to the parser
   --
--   type parser_buffer is abstract tagged limited null record;
--   type parser_ptr is access all parser_buffer'Class;
   --
   --  Methods that need to be provided by the parser_buffer.
   --
--   function get_char(self : parser_buffer) return Character is abstract;
--   function is_next_digit(self : parser_buffer) return Boolean is abstract;
--   procedure next_char(self : in out parser_buffer) is abstract;
--   function not_end(self : parser_buffer) return Boolean is abstract;
--   function is_end(self : parser_buffer) return Boolean is abstract;
--   procedure next_line(self : in out parser_buffer) is abstract;
--   function request_more(self : in out parser_buffer) return Boolean is abstract;
--   function is_eof(self : in out parser_buffer) return Boolean is abstract;
--   procedure get_line(self : in out parser_buffer) is abstract;
   --
   --  The main parser function.  Returns True if parsing is successful.
   --
   function parse(buff : parser_ptr; e : out element_type) return Boolean;
   --
   --  Parser utility functions
   --
   --  Is character white space (spaces, tabs, carriage return, or line feed).
   --
   function isWhitespace(c : Character) return Boolean is
     ((c = ' ') or (c = Character'Val(10)) or (c = Character'Val(13)) or
          c = Character'Val(9))
       with Global => Null;
   pragma Pure_Function(isWhitespace);
   --
   --  Is character a decimal digit?
   --
   function isDigit(c : Character) return Boolean is (c >= '0' and c <= '9')
     with Global => Null;
   pragma Pure_Function(isDigit);
   --
   --  Is character an alphabetic character
   --
   function isAlpha(c : Character) return Boolean is
     ((c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z'))
     with Global => Null;
   pragma Pure_Function(isAlpha);
   --
   -- Is character a hexidecimal digit?
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
     with Global => Null;
   pragma Pure_Function(isHex);
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return uint32
     with Global => Null;
   pragma Pure_Function(hexDigit);
private
   --
   --  Utilities to assist in parsing
   --
   function append_to_list(head : cons_index; e : element_type) return Boolean;
   --
   --  Subfunction for parsing lists.  If the buffer ends before the end of the
   --  list is reached, more input is read and the parsing continues.
   --    buff   - Buffer object to parse
   --    s_expr - Parsed s expression
   --    qfixed - The list is quoted
   --
   function list(buff : parser_ptr; s_expr : out cons_index;
                 qfixed : Boolean; base : Boolean)
                 return Boolean;
   --
   --  Subfunction for parsing symbols or temp symbols.  A is an atom that points
   --  to either the symbol or temp symbol.  Returns false if the symbol or temp
   --  symbol can't be found or created or if the atom can't be created.
   --
   function symb(buff : parser_ptr; quoted : Boolean)
                 return element_type;
   --
   --  Subfunction for parsing integers
   --
   procedure int(buff : parser_ptr; value : out int32)
     with Global => Null;
   --
   --  Hexidecimal numbers are read in as 32 bit unsigned integers and conerted
   --  to signed 32 bit integers using an unchecked conversion.
   --
   procedure hex(buff : parser_ptr; value : out int32)
     with Global => Null;
   --
   --  Parse strings
   --
   function parse_str(buff : parser_ptr; s : out string_index) return Boolean;
   --
   --  Parse characters
   --
   function parse_char(buff : parser_ptr; c : out Character) return Boolean
     with Global => Null;
end;

