with BBS.lisp.memory;
with BBS.lisp.utilities;
package body BBS.lisp.evaluate is
   --
   function execute_block(e : element_type) return element_type is
      statement : element_type;
      ret_val : element_type;
   begin
      --
      --  Evaluate the function
      --
      statement := e;
      ret_val := NIL_ELEM;
      while BBS.lisp.utilities.isList(statement) loop
         BBS.lisp.memory.deref(ret_val);
         if BBS.lisp.utilities.isList(cons_table(statement.ps).car) then
            ret_val := eval_dispatch(BBS.lisp.utilities.getList(cons_table(statement.ps).car));
            if ret_val.kind = E_ERROR then
               error("block execution", "Operation returned an error");
               exit;
            end if;
         else
            ret_val := cons_table(statement.ps).car;
         end if;
         statement := cons_table(statement.ps).cdr;
      end loop;
      return ret_val;
   end;
   --
end;
