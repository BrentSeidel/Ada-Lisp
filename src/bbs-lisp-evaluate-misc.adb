package body BBS.lisp.evaluate.misc is
   --
   function reset(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      init;
      return NIL_ELEM;
   end;
   --
   function msg_on(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      msg_flag := True;
      return NIL_ELEM;
   end;
   --
   function msg_off(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      msg_flag := False;
      return NIL_ELEM;
   end;
   --
   function dump(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      dump_cons;
      dump_symbols;
      dump_strings;
      return NIL_ELEM;
   end;
   --
   --  Set the quit flag to exit the lisp interpreter
   --
   function quit(e : element_type) return element_type is
      pragma Unreferenced (e);
   begin
      exit_flag := True;
      return NIL_ELEM;
   end;
   --
end;
