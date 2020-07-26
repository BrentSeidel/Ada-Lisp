--with bbs.lisp.memory;
--
--  This package handles the parsing of input.  Input can be a list, an atom
--  of either a symbol or integer, or a comment.
--
private package bbs.lisp.parser is
   --
   --  Utilities to assist in parsing
   --
   type TOKEN_TYPE is (LIST, TOK_SYMB, TOK_NUMBER, INVALID);
   --
   --  The main parser function
   --
   function parse(buff : in out String; last : in out Integer; e : out element_type) return Boolean;
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
   function int(ptr : in out Integer; buff : String; last : Integer; value : out Integer)
                return Boolean;
   --
   --  Parse strings
   --
   function parse_str(ptr : in out Integer; buff : in String;
                      last : in integer; s : out string_index) return Boolean;
   --
   --  Procedure to skip white space
   --
   procedure skip_whitespace(ptr : in out Integer; buff : String; last : Integer);
   --
   --  Function to determine if a character is a digit or not
   --
   function Is_Digit(c : Character) return Boolean;
end;
