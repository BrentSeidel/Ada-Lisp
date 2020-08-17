--with bbs.lisp.memory;
--
--  This package handles the parsing of input.  Input can be a list, an atom
--  of either a symbol or integer, or a comment.
--
private package bbs.lisp.parser is
   --
   --  The main parser function
   --
   function parse(buff : in out String; last : in out Integer; e : out element_type) return Boolean;
private
   --
   --  Utilities to assist in parsing
   --
   function append_to_list(head : cons_index; e : element_type) return Boolean;
   --
   --  Subfunction for parsing lists
   --
   function list(ptr : in out Integer; buff : in out String; last : in out Integer; s_expr : out cons_index)
                 return Boolean;
   --
   --  Subfunction for parsing symbols or temp symbols.  A is an atom that points
   --  to either the symbol or temp symbol.  Returns false if the symbol or temp
   --  symbol can't be found or created or if the atom can't be created.
   --
   function symb(ptr : in out Integer; buff : String; last : Integer)
                 return element_type;
   --
   --  Subfunction for parsing integers
   --
   function int(ptr : in out Integer; buff : String; last : Integer; value : out int32)
                return Boolean;
   --
   --  Hexidecimal numbers are read in as 32 bit unsigned integers and conerted
   --  to signed 32 bit integers using an unchecked conversion.
   --
   function hex(ptr : in out Integer; buff : String; last : Integer; value : out int32)
                return Boolean;
   --
   --  Parse strings
   --
   function parse_str(ptr : in out Integer; buff : in String;
                      last : in integer; s : out string_index) return Boolean;
   --
   --  Parse characters
   --
   function parse_char(ptr : in out Integer; buff : in String;
                       last : in Integer; c : out Character) return Boolean;
   --
   --  Procedure to skip white space
   --
   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer);
   --
   --  Function to determine if a character is a digit or not in different number
   --  systems.
   --
   function isDigit(c : Character) return Boolean;
   function isHex(c : Character) return Boolean;
   function hexDigit(c : Character) return uint32;
end;
