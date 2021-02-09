--
--  This package is a simple embeddable tiny lisp interpreter.  With being able
--  to load Ada programs directly onto Arm based embedded computers, it seems
--  like it might be useful to be able to embed some sort of interpreter to
--  help test and explore the device without having to constantly edit, build,'
--  and load the Ada program.
--
--  Basic lisp operations will be supported.  In addition, predefined functions
--  can be added to directly interface with the hardware.  Since each board is
--  different, these will have to be customized for each target.
--
with Ada.Unchecked_Conversion;
package bbs.lisp
with Abstract_State => (pvt_exit_flag, pvt_break_flag, pvt_string_table,
                        pvt_msg_flag, pvt_exit_loop, pvt_first_char_flag) is
   --
   --  Define the basic types used.
   --
   --  Sizes for the global data structures.  These can be adjusted as needed.
   --
   max_cons : constant Integer := 500;
   max_symb : constant Integer := 300;
   max_string : constant Integer := 500;
   max_stack : constant Integer := 100;
   --
   type cons_index is range -1 .. max_cons;
   type symb_index is range -1 .. max_symb;
   type string_index is range -1 .. max_string;
   type stack_index is range -1 .. max_stack;
   --
   --  This indicates what type of an object an element_type is pointing to.  It
   --  can be a cons cell, a value, a symbol, a temporary symbol a stack
   --  variable, or nothing.
   --
   type ptr_type is (E_CONS, E_ERROR, E_NIL, E_STACK, E_SYMBOL,
                     E_TEMPSYM, E_VALUE);
   --
   --  This indicates what kind of data is in a value.  These are the allowed
   --  data types.
   --
   type value_type is (V_INTEGER, V_STRING, V_CHARACTER, V_BOOLEAN, V_LIST,
                      V_LAMBDA, V_SYMBOL, V_QSYMBOL, V_NONE);
   --
   --  This indicates what kind of data is in a symbol.
   --
   type symbol_type is (SY_SPECIAL,  -- A special form that need support during parsing
                        SY_BUILTIN,  -- A normal builtin function
                        SY_LAMBDA,   -- A user defined function
                        SY_VARIABLE, -- A value, not a function
                        SY_EMPTY);   -- No contents

   --
   --  Phase of operation.  Some functions will need to know.  This is mainly
   --  used to allow a function to be able to build stack frames so that parsing
   --  of nested functions can properly identify parameters or local variables.
   --
   --  PH_QUERY       - Ask the routine when it wants to be called again.
   --  PH_PARSE_BEGIN - At desired point in parsing.
   --  PH_PARSE_END   - At the end of parsing.
   --  PH_EXECUTE     - Normal execution
   --
   type phase is (PH_QUERY, PH_PARSE_BEGIN, PH_PARSE_END, PH_EXECUTE);
   --
   --  Define the 32 bit signed and unsigned integer types along with unchecked
   --  conversions.  This is to support bitwise logical operations..
   --
   type int32 is range -(2**31) .. 2**31 - 1
     with Size => 32;
   type uint32 is mod 2**32
     with Size => 32;
   function uint32_to_int32 is
      new Ada.Unchecked_Conversion(source => uint32, target => int32);
   function int32_to_uint32 is
      new Ada.Unchecked_Conversion(source => int32, target => uint32);

   --
   --  Define the contents of records.
   --
   type value(kind : value_type := V_INTEGER) is
      record
         case kind is
         when V_INTEGER =>
            i : int32;
         when V_CHARACTER =>
            c : Character;
         when V_STRING =>
            s : string_index;
         when V_BOOLEAN =>
            b : Boolean;
         when V_LIST =>
            l : cons_index;
         when V_LAMBDA =>
            lam : cons_index;
         when V_SYMBOL =>
            sym : symb_index;
         when V_QSYMBOL =>
            qsym : symb_index;
         when V_NONE =>
            null;
         end case;
      end record;
   --
   --  An element_type can contain a value or point to a cons cell.
   --
   type element_type(kind : ptr_type := E_NIL) is
      record
         case kind is
            when E_CONS =>
               ps : cons_index;
            when E_ERROR =>
               null;
            when E_NIL =>
               null;
            when E_TEMPSYM =>
               tempsym : string_index;
            when E_SYMBOL =>
               sym : symb_index;
            when E_STACK =>
               st_name : string_index;
               st_offset : Natural;
            when E_VALUE =>
               v : value;
         end case;
      end record;
   --
   --  A cons cell contains two element_type pointers that can point to either
   --  an atom or another cons cell.
   --
   type cons is
      record
         ref : Natural;
         car : element_type;
         cdr : element_type;
      end record;
   --
   --  Define function types for Ada.Text_IO replacements.
   --
   type t_put_line is access procedure(s : String);
   type t_newline is access procedure;
   type t_get_line is access procedure(Item : out String; Last : out Natural);
   --
   --  Type for access to function that implement lisp operations.
   --
   type execute_function is access function(s : cons_index) return element_type;
   --
   --  Type for access to functions that implement lisp special operations
   --
   type special_function is access function(s : cons_index; p : phase) return element_type;
   --
   --  A symbol give a perminant name to a piece of data.  These can be global
   --  variables, user defined functions, or builtin functions.  The builtin
   --  functions are predefined and cannot be changed.
   --  A symbol record contains a name and a type
   --
   type symbol(kind : symbol_type := SY_EMPTY) is
      record
         ref : Natural;
         str : string_index;
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
   --  The main data tables for various kinds of data.
   --
   --
   --  Since this interpreter is designed to be used on embedded computers with
   --  no operating system and possibly no dynamic memory allocation, The
   --  statically allocated data structures are defined here.
   --
   cons_table : array (cons_index'First + 1 .. cons_index'Last) of cons;
   symb_table : array (symb_index'First + 1 .. symb_index'Last) of symbol;
   --
   --  Do initialization and define text I/O routines
   --
   procedure init(p_put_line : t_put_line; p_put : t_put_line;
                  p_new_line : t_newline; p_get_line : t_get_line);
   --
   --  The read procedure/function reads the complete text of an s-expression
   --  from some input device.  The string is then parsed into a binary form
   --  that can be evaluated.
   --
   function read return Element_Type;
   --
   --  This procedure evaluates a binary s-expression and returns the resuls.
   --
   function eval(e : element_type) return element_type;
   --
   --  For ease of embedding, this implements the full read-evaluate-print loop.
   --
   procedure repl;
   --
   --  This function checks if the lisp read-eval-print loop should exit
   --
   function exit_lisp return Boolean
     with Global => (Input => pvt_exit_flag);
   --
   --  Create a symbol for a builtin function.  This is intended to be called
   --  during initialization to identify the builtin operations.  Once created,
   --  these should never be changed.  No value is returned.
   --
   procedure add_builtin(n : String; f : execute_function)
     with Global => (In_Out => (symb_table, pvt_string_table));
   --
   --  Procedures for printing error and non-error messages.  Pass in string
   --  representing the function name and the message.  This is intended to
   --  make error messages more consistent.
   --
   procedure error(f : String; m : String)
     with Global => Null;
   --  The output stream is written to.
   procedure msg(f : String; m : String)
     with Global => (Input => pvt_msg_flag);
   --  The output stream is written to.
   procedure print(e : element_type; d : Boolean; nl : Boolean)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   --  The output stream is written to.
   --
   --  Some useful constants
   --
   NIL_ELEM : constant element_type := (Kind => E_NIL);
   NIL_CONS : constant cons_index := cons_index'First;
   NIL_STR : constant string_index := string_index'First;
   NIL_SYM : constant symb_index := symb_index'First;

private
   --
   --  Global flags
   --
   --  Flag for exiting the REPL
   --
   exit_flag : Boolean := False
     with Part_Of => pvt_exit_flag;
   --
   --  Flag to exit current command
   --
   break_flag : Boolean := False
     with Part_Of => pvt_break_flag;
   --
   --  Flag to enable or disable display of messages
   --
   msg_flag : Boolean := False
     with Part_Of => pvt_msg_flag;
   --
   --  Set to non-zero to break out of that many nested loops
   --
   exit_loop : Natural
     with Part_Of => pvt_exit_loop;
   --
   --  Will printing start on a new line
   --
   first_char_flag : Boolean := True
     with Part_Of => pvt_first_char_flag;
   --
   --  Structures and definitions for handling strings
   --
   fragment_len : constant Integer := 16;
   type fragment is
      record
         ref : Natural;
         next : string_index;
         len : Integer range 0..fragment_len;
         str : String (1..fragment_len);
      end record;
   --
   --
   string_table : array (string_index'First + 1 .. string_index'Last) of fragment
     with Part_Of => pvt_string_table;
   --
   --  Initialize the data structures used in the lisp interpreter.
   --
   procedure init;
   --
   --  Define some enumerations
   --
   --
   --  Type to indicate the result of comparisons.  The CMP_NE options is
   --  available for comparisons that cannot be ordered, otherwise CMP_LT or
   --  CMP_GT should be used.
   --
   type comparison is (CMP_EQ, CMP_LT, CMP_GT, CMP_NE);
   --
   --  Operations for math and comparisons
   --
   type mathops is (PLUS, MINUS, MUL, DIV);
   type compops is (SYM_EQ, SYM_NE, SYM_LT, SYM_GT);
   --
   --  These procedures print various types of objects.
   --
   procedure print(s : cons_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure print(v : value)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure print(s : string_index)
     with Global => (Input => pvt_string_table);
   procedure print(s : symb_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   --
   --  For debugging, dump the various tables
   --
   procedure dump_cons
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump_symbols
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump_strings
     with Global => (Input => pvt_string_table);
   procedure dump(e : element_type)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(s : cons_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(s : symb_index)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   procedure dump(v : value)
     with Global => (Input => (cons_table, symb_table, pvt_string_table));
   --
   --  Local functions and procedures
   --
   --
   --  Function pointers for I/O.  Using these pointers may allow the interpeter
   --  to be run on systems without access to Ada.Text_IO.
   --
   io_put_line : t_put_line;
   io_put      : t_put_line;
   io_new_line : t_newline;
   io_get_line : t_get_line;
   --
   --  Replacements for Text_IO to make porting to embedded systems easier.
   --  These call the user specified routines in the above pointers.
   --
   procedure put_line(s : String)
     with Global => (Output => pvt_first_char_flag);
   procedure put(s : String)
     with Global => (Output => pvt_first_char_flag);
   procedure new_line
     with Global => (Output => pvt_first_char_flag);
   procedure Get_Line(Item : out String; Last : out Natural)
     with Global => (Output => pvt_first_char_flag);
   --
   --  Functions for symbols.
   --
   --  If a symbol exists, return it, otherwise create a new symbol.  Returns
   --  false if symbol doesn't exist and can't be created.
   --
   function get_symb(s : out symb_index; n : String) return Boolean
     with Global => (Input => symb_table);
   --  It really is (In_Out => symb_table)
   function get_symb(s : out symb_index; n : string_index) return Boolean
     with Global => (Input => (symb_table, pvt_string_table));
   --  It really is (In_Out => (symb_table, pvt_string_table))
   --
   --  Finds a symbol and returns it.  Returns false if symbol can't be found.
   --
   function find_variable(n : string_index; create : Boolean) return element_type;
   --
   --  Create a symbol for a special function.  This is intended to be called
   --  during initialization to identify the special operations.  Once created,
   --  these should never be changed.  No value is returned.
   --
   procedure add_special(n : String; f : special_function)
     with Global => (In_Out => (symb_table, pvt_string_table));
   --
   --  Utility functions for manipulating lists
   --
   function elem_to_cons(s : out cons_index; e : element_type) return Boolean;
   function append(s1 : cons_index; s2 : cons_index) return Boolean;
   --
   --  Function for dispatching the various functions for evaluation.
   --
   function eval_dispatch(s : cons_index) return element_type;


end bbs.lisp;
