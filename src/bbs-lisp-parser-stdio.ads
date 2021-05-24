--
--  This package contains data and routines for providing data to the parser
--  routines.  This is done to allow the parser to be decoupled from I/O.
--
package bbs.lisp.parser.stdio is
   type parser_stdio is new parser_buffer with
      record
         buff : String(1 .. 256);
         ptr : Integer;
         last : Integer;
      end record;
   type parser_stdio_ptr is access all parser_stdio'Class;
   --
   overriding
   function get_char(self : parser_stdio) return Character is (self.buff(self.ptr));
   --
   overriding
   function get_next_char(self : parser_stdio) return Character is(self.buff(self.ptr + 1));
   --
   overriding
   procedure next_char(self : in out parser_stdio);
   --
   overriding
   function not_end(self : parser_stdio) return Boolean is (self.ptr <= self.last);
   --
   overriding
   function is_end(self : parser_stdio) return Boolean is (self.ptr > self.last);
   --
   overriding
   procedure set_end(self : in out parser_stdio);
   --
   overriding
   procedure get_line(self : in out parser_stdio);
   --
   overriding
   procedure request_more(self : in out parser_stdio);
   --
   procedure init(self : in out parser_stdio);

end;
