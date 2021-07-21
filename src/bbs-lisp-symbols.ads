--
--  This package contains the data structures and routines for accessing the
--  symbol table.
--
with BBS.lisp.evaluate.bool;
with BBS.lisp.evaluate.char;
with BBS.lisp.evaluate.cond;
with BBS.lisp.evaluate.func;
with BBS.lisp.evaluate.io;
with BBS.lisp.evaluate.list;
with BBS.lisp.evaluate.loops;
with BBS.lisp.evaluate.math;
with BBS.lisp.evaluate.mem;
with BBS.lisp.evaluate.misc;
with BBS.lisp.evaluate.pred;
with BBS.lisp.evaluate.str;
with BBS.lisp.evaluate.symb;
with BBS.lisp.evaluate.vars;
package BBS.lisp.symbols is
   --
   --  All symbols have the same body, so to be consistent we define it here.
   --
   type sym_body(Kind : symbol_type := SY_EMPTY) is
      record
         case kind is
            when SY_SPECIAL =>
               s : special_function;
            when SY_BUILTIN =>
               f : execute_function;
            when SY_LAMBDA =>
               ps : cons_index;
            when SY_VARIABLE =>
               pv : element_type;
            when SY_EMPTY =>
               null;
         end case;
      end record;
   --
   --  A symbol give a perminant name to a piece of data.  These can be global
   --  variables, user defined functions, or builtin functions.  The builtin
   --  functions are predefined and cannot be changed.
   --  A symbol record contains a name and a type
   --
   type symbol(kind : symbol_type := SY_EMPTY) is
      record
         ref : Natural;
         name : string_index;
         b    : sym_body(kind);
--         case kind is
--            when SY_SPECIAL =>
--               s : special_function;
--            when SY_BUILTIN =>
--               f : execute_function;
--            when SY_LAMBDA =>
--               ps : cons_index;
--            when SY_VARIABLE =>
--               pv : element_type;
--            when SY_EMPTY =>
--               null;
--         end case;
      end record;
   --
   --  Fixed symbols are like regular symbols except that the name is an Ada
   --  string and there is no reference count.
   --
      type fixed_symbol(kind : symbol_type := SY_EMPTY) is
      record
         name : access constant String;
         b    : sym_body(kind);
      end record;
   --
   --  Operations for symbols
   --
   --  Get the type of the symbol
   --
   function get_type(s : symb_index) return symbol_type
     with pre => (s /= NIL_SYM);
   --
   function get_type(s : symbol_ptr) return symbol_type
     with pre => (s.kind /= ST_NULL);
   --
   --  Check if symbol is fixed (builtin or special)
   --
   function isFixed(s : symb_index) return Boolean
     with pre => (s /= NIL_SYM);
   --
   function isFixed(s : symbol_ptr) return Boolean
     with pre => (s.kind /= ST_NULL);
   --
   --  Check if symbol is a function (builtin, special, or user defined function)
   --
   function isFunction(s : symb_index) return Boolean
     with pre => (s /= NIL_SYM);
   --
   function isFunction(s : symbol_ptr) return Boolean
     with pre => (s.kind /= ST_NULL);
   --
   --  If symbol is a variable, get the symbol value.
   --
   function get_value(s : symb_index) return element_type
     with pre => (get_type(s) = SY_VARIABLE);
   --
   function get_value(s : symbol_ptr) return element_type
     with pre => (get_type(s) = SY_VARIABLE);
   --
   --  If symbol is a lambda, get the list.
   --
   function get_list(s : symb_index) return cons_index
     with pre => (get_type(s) = SY_LAMBDA);
   --
   function get_list(s : symbol_ptr) return cons_index
     with pre => (get_type(s) = SY_LAMBDA);
   --
   --  Get a symbol's name (there are two different routines because the name is
   --  stored differently between fixed and dynamic symbols.
   --
   function get_name(s : symb_index) return string_index
     with pre => (s /= NIL_SYM);
   --
   function get_name(s : symbol_ptr) return string_index
     with pre => (s.kind = ST_DYNAMIC);
   --
   function get_name(s : symbol_ptr) return access constant String
     with pre => (s.kind = ST_FIXED);
   --
   --  Get a symbol's reference count
   --
   function get_ref(s : symb_index) return Natural
     with pre => (s /= NIL_SYM);
   --
   function get_ref(s : symbol_ptr) return Natural
     with pre => (s.kind /= ST_NULL);
   --
   --  Get a symbol from the symbol table
   --
   function get_sym(s : symb_index) return symbol
     with pre => (s /= NIL_SYM);
   --
   function get_sym(s : symbol_ptr) return sym_body
     with pre => (s.kind /= ST_NULL);
   --
   --  Set a symbol entry
   --
   procedure set_sym(s : symb_index; val : symbol)
     with pre => (s /= NIL_SYM);
   --
   procedure set_sym(s : symbol_ptr; val : sym_body)
     with pre => (s.kind = ST_DYNAMIC);
   --
   --  Add a new symbol entry
   --
   procedure add_sym(s : symbol_ptr; val : symbol)
     with pre => ((s.kind = ST_DYNAMIC) and (get_ref(s) = 0));
   --
   --  Search the symbol table for a name
   --
   function find_name(s : string_index) return symbol_ptr;
   --
   --  Reset the symbol table
   --
   procedure reset_symbol_table;
   --
private
   --
   --  The symbol table.
   --
   symb_table : array (symb_index'First + 1 .. symb_index'Last) of symbol;
   --
   --
   --  Define the constant strings for the fixed symbol table.
   --
   ADD                 : aliased constant String := "+";
   SUB                 : aliased constant String := "-";
   MUL                 : aliased constant String := "*";
   DIV                 : aliased constant String := "/";
   EQ                  : aliased constant String := "=";
   NE                  : aliased constant String := "/=";
   LT                  : aliased constant String := "<";
   GT                  : aliased constant String := ">";
   EVAL_AND            : aliased constant String := "AND";
   ARRAYP              : aliased constant String := "ARRAYP";
   ATOMP               : aliased constant String := "ATOMP";
   BIT_VECTOR_P        : aliased constant String := "BIT-VECTOR-P";
   CAR                 : aliased constant String := "CAR";
   CDR                 : aliased constant String := "CDR";
   CHAR                : aliased constant String := "CHAR";
   CHAR_CODE           : aliased constant String := "CHAR-CODE";
   CHAR_DOWNCASE       : aliased constant String := "CHAR-DOWNCASE";
   CHAR_UPCASE         : aliased constant String := "CHAR-UPCASE";
   CHARACTERP          : aliased constant String := "CHARACTERP";
   CODE_CHAR           : aliased constant String := "CODE-CHAR";
   COERCE              : aliased constant String := "COERCE";
   COMPILED_FUNCTION_P : aliased constant String := "COMPILED-FUNCTION-P";
   COMPLEXP            : aliased constant String := "COMPLEXP";
   CONCATENATE         : aliased constant String := "CONCATENATE";
   CONS                : aliased constant String := "CONS";
   CONSP               : aliased constant String := "CONSP";
   DEFUN               : aliased constant String := "DEFUN";
   DOLIST              : aliased constant String := "DOLIST";
   DOTIMES             : aliased constant String := "DOTIMES";
   DOWHILE             : aliased constant String := "DOWHILE";
   DUMP                : aliased constant String := "DUMP";
   ERRORP              : aliased constant String := "ERRORP";
   EVAL                : aliased constant String := "EVAL";
   EXIT_LISP           : aliased constant String := "EXIT";
   FLOATP              : aliased constant String := "FLOATP";
   FRESH_LINE          : aliased constant String := "FRESH-LINE";
   FUNCTIONP           : aliased constant String := "FUNCTIONP";
   EVAL_IF             : aliased constant String := "IF";
   INTEGERP            : aliased constant String := "INTEGERP";
   LAMBDA              : aliased constant String := "LAMBDA";
   LENGTH              : aliased constant String := "LENGTH";
   LET                 : aliased constant String := "LET";
   LIST                : aliased constant String := "LIST";
   LISTP               : aliased constant String := "LISTP";
   MSG                 : aliased constant String := "MSG";
   LISP_F              : aliased constant String := "NIL";
   EVAL_NOT            : aliased constant String := "NOT";
   NULLP               : aliased constant String := "NULL";
   NUMBERP             : aliased constant String := "NUMBERP";
   EVAL_OR             : aliased constant String := "OR";
   PACKAGEP            : aliased constant String := "PACKAGEP";
   PARSE_INT           : aliased constant String := "PARSE-INTEGER";
   PEEK8               : aliased constant String := "PEEK8";
   PEEK16              : aliased constant String := "PEEK16";
   PEEK32              : aliased constant String := "PEEK32";
   POKE8               : aliased constant String := "POKE8";
   POKE16              : aliased constant String := "POKE16";
   POKE32              : aliased constant String := "POKE32";
   PRINT               : aliased constant String := "PRINT";
   PROGN               : aliased constant String := "PROGN";
   QUOTE               : aliased constant String := "QUOTE";
   RATIONALP           : aliased constant String := "RATIONALP";
   READ                : aliased constant String := "READ";
   READ_LINE           : aliased constant String := "READ-LINE";
   REALP               : aliased constant String := "REALP";
   RETURN_FROM         : aliased constant String := "RETURN";
   SETQ                : aliased constant String := "SETQ";
   SIMPLE_BIT_VECTOR_P : aliased constant String := "SIMPLE-BIT-VECTOR-P";
   SIMPLE_STRING_P     : aliased constant String := "SIMPLE-STRING-P";
   SIMPLE_VECTOR_P     : aliased constant String := "SIMPLE-VECTOR-P";
   SLEEP               : aliased constant String := "SLEEP";
   STRING_DOWNCASE     : aliased constant String := "STRING-DOWNCASE";
   STRING_UPCASE       : aliased constant String := "STRING-UPCASE";
   STRINGP             : aliased constant String := "STRINGP";
   SUBSEQ              : aliased constant String := "SUBSEQ";
   SYMBOLP             : aliased constant String := "SYMBOLP";
   LISP_T              : aliased constant String := "T";
   TERPRI              : aliased constant String := "TERPRI";
   VECTORP             : aliased constant String := "VECTORP";
   --
   --  The fixed symbol table.
   --
   type index_map is array (fsymb_index range <>) of fixed_symbol;
   index : constant index_map :=
     (
        (name => ADD'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.math.add'Access)),
      (name => SUB'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.math.sub'Access)),
      (name => MUL'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.math.mul'Access)),
      (name => DIV'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.math.div'Access)),
      (name => EQ'Access,                  Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.cond.eq'Access)),
      (name => NE'Access,                  Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.cond.ne'Access)),
      (name => LT'Access,                  Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.cond.lt'Access)),
      (name => GT'Access,                  Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.cond.gt'Access)),
      (name => EVAL_AND'Access,            Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.bool.eval_and'Access)),
      (name => ARRAYP'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => ATOMP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.atomp'Access)),
      (name => BIT_VECTOR_P'Access,        Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => CAR'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.list.car'Access)),
      (name => CDR'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.list.cdr'Access)),
      (name => CHAR'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.char'Access)),
      (name => CHAR_CODE'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.char.char_code'Access)),
      (name => CHAR_DOWNCASE'Access,       Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.char.char_downcase'Access)),
      (name => CHAR_UPCASE'Access,         Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.char.char_upcase'Access)),
      (name => CHARACTERP'Access,          Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.characterp'Access)),
      (name => CODE_CHAR'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.char.code_char'Access)),
      (name => COERCE'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.symb.coerce'Access)),
      (name => COMPILED_FUNCTION_P'Access, Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.compiled_function_p'Access)),
      (name => COMPLEXP'Access,            Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => CONCATENATE'Access,         Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.symb.concatenate'Access)),
      (name => CONS'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.list.cons'Access)),
      (name => CONSP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.consp'Access)),
      (name => DEFUN'Access,               Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.func.defun'Access)),
      (name => DOLIST'Access,              Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.loops.dolist'Access)),
      (name => DOTIMES'Access,             Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.loops.dotimes'Access)),
      (name => DOWHILE'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.loops.dowhile'Access)),
      (name => DUMP'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.misc.dump'Access)),
      (name => ERRORP'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.errorp'Access)),
      (name => EVAL'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.func.eval_list'Access)),
      (name => EXIT_LISP'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.misc.quit'Access)),
      (name => FLOATP'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => FRESH_LINE'Access,          Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.io.fresh_line'Access)),
      (name => FUNCTIONP'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.functionp'Access)),
      (name => EVAL_IF'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.cond.eval_if'Access)),
      (name => INTEGERP'Access,            Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.integerp'Access)),
      (name => LAMBDA'Access,              Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.func.lambda'Access)),
      (name => LENGTH'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.length'Access)),
      (name => LET'Access,                 Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.vars.local'Access)),
      (name => LIST'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.list.list'Access)),
      (name => LISTP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.listp'Access)),
      (name => MSG'Access,                 Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.misc.msg'Access)),
      (name => LISP_F'Access,              Kind => SY_VARIABLE, b => (Kind => SY_VARIABLE, pv => (kind => E_VALUE, v => (kind => V_BOOLEAN, b => False)))),
      (name => EVAL_NOT'Access,            Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.bool.eval_not'Access)),
      (name => NULLP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.nullp'Access)),
      (name => NUMBERP'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.numberp'Access)),
      (name => EVAL_OR'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.bool.eval_or'Access)),
      (name => PACKAGEP'Access,            Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => PARSE_INT'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.parse_integer'Access)),
      (name => PEEK8'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.peek8'Access)),
      (name => PEEK16'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.peek16'Access)),
      (name => PEEK32'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.peek32'Access)),
      (name => POKE8'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.poke8'Access)),
      (name => POKE16'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.poke16'Access)),
      (name => POKE32'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.mem.poke32'Access)),
      (name => PRINT'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.io.print'Access)),
      (name => PROGN'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.loops.progn'Access)),
      (name => QUOTE'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.list.quote'Access)),
      (name => RATIONALP'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => READ'Access,                Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.io.read_expr'Access)),
      (name => READ_LINE'Access,           Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.io.read_line'Access)),
      (name => REALP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => RETURN_FROM'Access,         Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.loops.return_from'Access)),
      (name => SETQ'Access,                Kind => SY_SPECIAL, b => (Kind => SY_SPECIAL, s => BBS.lisp.evaluate.vars.setq'Access)),
      (name => SIMPLE_BIT_VECTOR_P'Access, Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => SIMPLE_STRING_P'Access,     Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.simple_string_p'Access)),
      (name => SIMPLE_VECTOR_P'Access,     Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access)),
      (name => SLEEP'Access,               Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.misc.sleep'Access)),
      (name => STRING_DOWNCASE'Access,     Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.string_downcase'Access)),
      (name => STRING_UPCASE'Access,       Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.string_upcase'Access)),
      (name => STRINGP'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.stringp'Access)),
      (name => SUBSEQ'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.str.subseq'Access)),
      (name => SYMBOLP'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.symbolp'Access)),
      (name => LISP_T'Access,              Kind => SY_VARIABLE, b => (Kind => SY_VARIABLE, pv => (kind => E_VALUE, v => (kind => V_BOOLEAN, b => True)))),
      (name => TERPRI'Access,              Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.io.terpri'Access)),
      (name => VECTORP'Access,             Kind => SY_BUILTIN, b => (Kind => SY_BUILTIN, f => BBS.lisp.evaluate.pred.return_false'Access))
     );
end;
