package BBS.lisp.evaluate.vars is

--   function setq(s : cons_index; p : phase) return element_type;
   procedure setq(e : out element_type; s : cons_index; p : phase);
   --
--   function local(s : cons_index; p : phase) return element_type;
   procedure local(e : out element_type; s : cons_index; p : phase);

end;
