--
--  This package handles the parsing of input.  Input can be a list, an atom
--  of either a symbol or integer, or a comment.
--
private package bbs.lisp.parser is
   --
   --  Define an abstract class for providing data to the parser
   --
   type parser_buffer is abstract tagged limited null record;
   type parser_ptr is access all parser_buffer'Class;
   --
   --  Methods that need to be provided by the parser_buffer.
   --
   function get_char(self : parser_buffer) return Character is abstract;
   function is_next_digit(self : parser_buffer) return Boolean is abstract;
   procedure next_char(self : in out parser_buffer) is abstract;
   function not_end(self : parser_buffer) return Boolean is abstract;
   function is_end(self : parser_buffer) return Boolean is abstract;
   procedure set_end(self : in out parser_buffer) is abstract;
   function request_more(self : in out parser_buffer) return Boolean is abstract;
   --
   --  The main parser function.  Returns True if parsing is successful.
   --
   function parse(buff : parser_ptr; e : out element_type) return Boolean
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   -- should be (In_Out => (cons_table, symb_table, pvt_string_table)
private
   --
   --  Utilities to assist in parsing
   --
   function append_to_list(head : cons_index; e : element_type) return Boolean
     with Global => (Input => cons_table);
   -- should be (In_Out => cons_table)
   --
   --  Subfunction for parsing lists.  If the buffer ends before the end of the
   --  list is reached, more input is read and the parsing continues.
   --    buff   - Buffer object to parse
   --    s_expr - Parsed s expression
   --    qfixed - The list is quoted
   --
   function list(buff : parser_ptr; s_expr : out cons_index;
                 qfixed : Boolean; base : Boolean)
                 return Boolean
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   -- should be (In_Out => (cons_table, symb_table, pvt_string_table)
   --
   --  Subfunction for parsing symbols or temp symbols.  A is an atom that points
   --  to either the symbol or temp symbol.  Returns false if the symbol or temp
   --  symbol can't be found or created or if the atom can't be created.
   --
   function symb(buff : parser_ptr; quoted : Boolean)
                 return element_type
     with Global => (Input => (symb_table, pvt_string_table));
   -- should be (In_Out => (symb_table, pvt_string_table)
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
   function parse_str(buff : parser_ptr; s : out string_index) return Boolean
     with Global => (Input => pvt_string_table);
   -- should be (In_Out => string_table);
   --
   --  Parse characters
   --
   function parse_char(buff : parser_ptr; c : out Character) return Boolean
     with Global => Null;
   --
   --  Procedure to skip white space (spaces, tabs, carriage return, or line feed).
   --
   function isWhitespace(c : Character) return Boolean is
     ((c = ' ') or (c = Character'Val(10)) or (c = Character'Val(13)) or
          c = Character'Val(9))
     with Global => Null;
--   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer)
--     with Global => Null;
end;

