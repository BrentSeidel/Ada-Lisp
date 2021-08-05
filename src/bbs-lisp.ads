--
--  This package is a simple embeddable tiny lisp interpreter.  With being able
--  to load Ada programs directly onto Arm based embedded computers, it seems
--  like it might be useful to be able to embed some sort of interpreter to
--  help test and explore the device without having to constantly edit, build,'
--  and load the Ada program.
--
--  Basic lisp operations are be supported.  In addition, predefined functions
--  can be added to directly interface with the hardware.  Since each board is
--  different, these will have to be customized for each target.
--
with Ada.Unchecked_Conversion;
package BBS.lisp
with Abstract_State => (pvt_exit_flag, pvt_break_flag,
                        pvt_msg_flag, pvt_first_char_flag,
                        output_stream, input_stream, parse) is
   --
   --  Define the basic types used.
   --
   --  Sizes for the global data structures.  These can be adjusted as needed.
   --
   max_cons   : constant Integer := 500;
   max_symb   : constant Integer := 250;
   max_string : constant Integer := 450;
   max_stack  : constant Integer := 100;
   --
   type cons_index is range -1 .. max_cons;
   type symb_index is range -1 .. max_symb;
   type string_index is range -1 .. max_string;
   type fsymb_index is new Positive;
   type symbol_table is (ST_NULL, ST_FIXED, ST_DYNAMIC);
   --
   --  Pointer to a symbol.  This needs to be able to distinguish between symbols
   --  that are in the fixed table and the dynamic table.
   --
   type symbol_ptr(kind : symbol_table := ST_NULL) is
      record
         case kind is
            when ST_NULL =>
               null;
            when ST_FIXED =>
               f : fsymb_index;
            when ST_DYNAMIC =>
               d : symb_index;
         end case;
      end record;
   --
   --  This indicates what kind of data is in an element.  These are the allowed
   --  data types.
   --
   type value_type is (V_INTEGER, V_STRING, V_CHARACTER, V_BOOLEAN, V_LIST,
                       V_LAMBDA, V_TEMPSYM, V_SYMBOL, V_QSYMBOL, V_STACK, V_ERROR,
                       V_NONE);
   --
   --  This indicates what kind of data is in a symbol.
   --
   type symbol_type is (SY_SPECIAL,  -- A special form that need support during parsing
                        SY_BUILTIN,  -- A normal builtin function
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
   --  Error codes.  These will be filled out later.
   --
   type error_code is  (ERR_ADDON,       --  An error specific to an addon
                        ERR_ALLOCCONS,   --  Unable to allocate a cons
                        ERR_ALLOCSTR,    --  Unable to allocate a string
                        ERR_ALLOCSYM,    --  Unable to allocate a symbol
                        ERR_FEWPARAM,    --  Too few parameters
                        ERR_FIXSYM,      --  Attempt to change a fixed symbol
                        ERR_HARDWARE,    --  A hardware related error
                        ERR_NOPARAM,     --  No parameters passed when required
                        ERR_NOTSYM,      --  Symbol needed, but not provided
                        ERR_PARSE,       --  General parse failure
                        ERR_PARSECHAR,   --  Error during parsing of character
                        ERR_PARSELIST,   --  Error during parsing of list
                        ERR_PARSESPEC,   --  Error during parsing of special function
                        ERR_RANGE,       --  Parameter value is out of range
                        ERR_STACK,       --  Stack function reported an error
                        ERR_UNKNOWN,     --  Unknown error code
                        ERR_WRONGTYPE);  --  Wrong data type for parameter
   --
   --  Define the 32 bit signed and unsigned integer types along with unchecked
   --  conversions.  This is to support bitwise logical operations.
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
   --  Define the contents of the element.  This is the basic data element.  It
   --  can store scalar values or pointers to other data structures.
   --
   type element_type(kind : value_type := V_NONE) is
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
         when V_TEMPSYM =>
            tempsym : string_index;
         when V_SYMBOL =>
            sym : symbol_ptr;
         when V_QSYMBOL =>
            qsym : symbol_ptr;
         when V_STACK =>
            st_name : string_index;
            st_offset : Natural;
         when V_ERROR =>
            err : error_code;
         when V_NONE =>
            null;
         end case;
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
   type execute_function is access procedure(e : out element_type; s : cons_index);
   --
   --  Type for access to functions that implement lisp special operations
   --
   type special_function is access procedure(e : out element_type; s : cons_index; p : phase);
   --
   --  Do initialization and define text I/O routines
   --
   procedure init(p_put_line : t_put_line; p_put : t_put_line;
                  p_new_line : t_newline; p_get_line : t_get_line)
     with Global => (Output => (input_stream, pvt_first_char_flag));
   --
   --  The read procedure/function reads the complete text of an s-expression
   --  from some input device.  The string is then parsed into a binary form
   --  that can be evaluated.
   --
   function read return Element_Type
     with Global => (Input => (input_stream));
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
   procedure add_builtin(n : String; f : execute_function);
   --
   --  Procedures for printing error and non-error messages.  Pass in string
   --  representing the function name and the message.  This is intended to
   --  make error messages more consistent.
   --
   procedure error(f : String; m : String)
     with Global => (Output => output_stream);
   procedure msg(f : String; m : String)
     with Global => (Input => pvt_msg_flag,
                     output => output_stream);
   procedure print(e : element_type; d : Boolean; nl : Boolean)
     with Global => (output => output_stream);
   --
   --  Create an error value with the specified error code
   --
   function make_error(err : error_code) return element_type;
   --
   --  Some useful constants
   --
   NIL_ELEM : constant element_type := (kind => V_NONE);
   NIL_CONS : constant cons_index := cons_index'First;
   NIL_STR  : constant string_index := string_index'First;
   NIL_SYM  : constant symbol_ptr := (kind => ST_NULL);
   ELEM_T   : constant element_type := (kind => V_BOOLEAN, b => True);
   ELEM_F   : constant element_type := (kind => V_BOOLEAN, b => False);
   --
   --  Define some enumerations
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
   --  Prompts
   --
   prompt1 : constant String := "LISP> ";  --  Primary lisp prompt
   prompt2 : constant String := "More> ";  --  Prompt for more text

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
   --  Will printing start on a new line
   --
   first_char_flag : Boolean := True
     with Part_Of => pvt_first_char_flag;
   --
   --  These procedures print various types of objects.
   --
   procedure print(s : cons_index);
   procedure print(s : string_index);
   procedure print(s : symbol_ptr);
   --
   --  For debugging, dump the various tables
   --
   procedure dump_cons;
   procedure dump_symbols;
   procedure dump_strings;
   procedure dump_sym_ptr(s : symbol_ptr);
   --
   --  Local functions and procedures
   --
   --
   --  Function pointers for I/O.  Using these pointers may allow the interpeter
   --  to be run on systems without access to Ada.Text_IO.
   --
   io_put_line : t_put_line
     with Part_Of => output_stream;
   io_put      : t_put_line
     with Part_Of => output_stream;
   io_new_line : t_newline
     with Part_Of => output_stream;
   io_get_line : t_get_line
     with Part_Of => input_stream;
   --
   --  Replacements for Text_IO to make porting to embedded systems easier.
   --  These call the user specified routines in the above pointers.
   --
   procedure put_line(s : String)
     with Global => (input => output_stream,
                     Output => pvt_first_char_flag);
   procedure put(s : String)
     with Global => (Output => pvt_first_char_flag,
                     Input => output_stream);
   procedure new_line
     with Global => (Output => pvt_first_char_flag,
                     Input => output_stream);
   procedure Get_Line(Item : out String; Last : out Natural)
     with Global => (Output => pvt_first_char_flag,
                     Input => input_stream);
   --
   --  Functions for symbols.
   --
   --  If a symbol exists, return it, otherwise create a new symbol.  Returns
   --  false if symbol doesn't exist and can't be created.
   --
   function get_symb(s : out symbol_ptr; n : String) return Boolean;
   function get_symb(s : out symbol_ptr; n : string_index) return Boolean;
   --
   --
   --  Finds a symbol and returns it.  Returns false if symbol can't be found.
   --
   function find_variable(n : string_index; create : Boolean) return element_type;
   --
   --  Create a symbol for a special function.  This is intended to be called
   --  during initialization to identify the special operations.  Once created,
   --  these should never be changed.  No value is returned.
   --
   procedure add_special(n : String; f : special_function);
   --
   --  Utility functions for manipulating lists
   --
   function elem_to_cons(s : out cons_index; e : element_type) return Boolean;
   function append(s1 : cons_index; s2 : cons_index) return Boolean;
   --
   --  Function for dispatching the various functions for evaluation.
   --
   function eval_dispatch(s : cons_index) return element_type;


end BBS.lisp;
