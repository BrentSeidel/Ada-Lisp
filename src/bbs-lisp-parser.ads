--with bbs.lisp.memory;
--
--  This package handles the parsing of input.  Input can be a list, an atom
--  of either a symbol or integer, or a comment.
--
private package bbs.lisp.parser is
   --
   --  The main parser function
   --
   function parse(buff : in out String; last : in out Integer; e : out element_type) return Boolean
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
   --  Subfunction for parsing lists
   --
   function list(ptr : in out Integer; buff : in out String;
                 last : in out Integer; s_expr : out cons_index;
                 qfixed : Boolean)
                 return Boolean
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   -- should be (In_Out => (cons_table, symb_table, pvt_string_table)
   --
   --  Subfunction for parsing symbols or temp symbols.  A is an atom that points
   --  to either the symbol or temp symbol.  Returns false if the symbol or temp
   --  symbol can't be found or created or if the atom can't be created.
   --
   function symb(ptr : in out Integer; buff : String; last : Integer; quoted : Boolean)
                 return element_type
     with Global => (Input => (symb_table, pvt_string_table));
   -- should be (In_Out => (symb_table, pvt_string_table)
   --
   --  Subfunction for parsing integers
   --
   procedure int(ptr : in out Integer; buff : String; last : Integer; value : out int32)
     with Global => Null;
   --
   --  Hexidecimal numbers are read in as 32 bit unsigned integers and conerted
   --  to signed 32 bit integers using an unchecked conversion.
   --
   procedure hex(ptr : in out Integer; buff : String; last : Integer; value : out int32)
     with Global => Null;
   --
   --  Parse strings
   --
   function parse_str(ptr : in out Integer; buff : in String;
                      last : in integer; s : out string_index) return Boolean
     with Global => (Input => pvt_string_table);
   -- should be (In_Out => string_table);
   --
   --  Parse characters
   --
   function parse_char(ptr : in out Integer; buff : in String;
                       last : in Integer; c : out Character) return Boolean
     with Global => Null;
   --
   --  Procedure to skip white space
   --
   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer)
     with Global => Null;
end;
