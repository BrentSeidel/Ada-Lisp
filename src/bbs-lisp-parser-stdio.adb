package body bbs.lisp.parser.stdio is
   --
   --  Initialize the buffer object
   --
   procedure init(self : in out parser_stdio) is
   begin
      self.ptr := self.buff'First;
      self.last := self.buff'First;
   end;
   --
   --  Move pointer to point to the next character in the buffer
   --
   procedure next_char(self : in out parser_stdio) is
   begin
      self.ptr := self.ptr + 1;
   end;
   --
   --  Sets pointer to the end of the buffer
   --
   procedure set_end(self : in out parser_stdio) is
   begin
      self.ptr := self.last + 1;
   end;
   --
   --  Read a line into the buffer
   --
   procedure get_line(self : in out parser_stdio) is
   begin
      Get_Line(self.buff, self.last);
      self.ptr := self.buff'First;
   end;
end;
