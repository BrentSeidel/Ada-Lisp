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
package bbs.lisp is
   --
   --  Define the basic types used.
   --
   --  Sizes for the global data structures.  These can be adjusted as needed.
   --
   max_cons : constant Integer := 300;
   max_symb : constant Integer := 200;
   max_string : constant Integer := 500;
   max_stack : constant Integer := 100;
   --
   type cons_index is range 0 .. max_cons;
   type symb_index is range 0 .. max_symb;
   type string_index is range -1 .. max_string;
   type stack_index is range 0 .. max_stack;
   --
   type t_put_line is access procedure(s : String);
   type t_newline is access procedure;
   type t_get_line is access procedure(Item : out String; Last : out Natural);
   --
   --  This indicates what type of an object an element_type is pointing to.  It
   --  can be a cons cell, an atom, or nothing.
   --
   type ptr_type is (E_CONS, E_NIL, E_VALUE, E_SYMBOL,
                     E_TEMPSYM, E_PARAM, E_LOCAL);
   --
   --  This indicates what kind of data is in a value.
   --
   type value_type is (V_INTEGER, V_STRING, V_CHARACTER, V_BOOLEAN, V_LIST,
                      V_NONE);
   --
   --  This indicates what kind of data is in a symbol
   --
   type symbol_type is (SPECIAL,  -- A special form that need support during parsing
                        BUILTIN,  -- A normal builtin function
                        LAMBDA,   -- A user defined function
                        VARIABLE, -- A value, not a function
                        EMPTY);   -- No contents
   --
   --  Phase of operation.  Some functions will need to know.
   --
   type phase is (QUERY, PARSE_BEGIN, PARSE_END, EXECUTE);
   --
   --  Define the contents of records.
   --
   type value(kind : value_type := V_INTEGER) is
      record
         case kind is
         when V_INTEGER =>
            i : Integer;
         when V_CHARACTER =>
            c : Character;
         when V_STRING =>
            s : string_index;
         when V_BOOLEAN =>
            b : Boolean;
         when V_LIST =>
            l : cons_index;
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
            when E_NIL =>
               null;
            when E_VALUE =>
               v : value;
            when E_SYMBOL =>
               sym : symb_index;
            when E_TEMPSYM =>
               tempsym : string_index;
            when E_PARAM =>
               p_name : string_index;
               p_offset : stack_index;
            when E_LOCAL =>
               l_name : string_index;
               l_offset : stack_index;
         end case;
      end record;
   --
   --  Type for access to function that implement lisp operations.
   --
   type execute_function is access function(e : element_type) return element_type;
   --
   --  Type for access to functions that implement lisp special operations
   --
   type special_function is access function(e : element_type; p : phase) return element_type;
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
   --  A symbol give a perminant name to a piece of data.  These can be global
   --  variables, user defined functions, or builtin functions.  The builtin
   --  functions are predefined and cannot be changed.
   --  A symbol record contains a name and a type
   --
   type symbol(kind : symbol_type := EMPTY) is
      record
         ref : Natural;
         str : string_index;
         case kind is
            when SPECIAL =>
               s : special_function;
            when BUILTIN =>
               f : execute_function;
            when LAMBDA =>
               ps : cons_index;
            when VARIABLE =>
               pv : element_type;
            when EMPTY =>
               null;
         end case;
      end record;
   --
   --  The main data tables for various kinds of data.
   --
   --
   --  Since this interpreter is designed to be used on embedded computers with
   --  no operating system and possibly no dynamic memory allocation, The max
   --  sizes for statically allocated data structures is defined here.
   --
   cons_table : array (cons_index) of cons;
   symb_table : array (symb_index) of symbol;
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
   procedure error(f : String; m : String);
   procedure msg(f : String; m : String);
   procedure print(e : element_type; d : Boolean; nl : Boolean);
   --
   --  Some useful constants
   --
   NIL_ELEM : constant element_type := (Kind => E_NIL);

private
   --
   --  Global flags
   --
   --  Flag for exiting the REPL
   --
   exit_flag : Boolean := False;
   --
   --  Flag to exit current command
   --
   break_flag : Boolean := False;
   --
   --  Flag to enable or disable display of messages
   --
   msg_flag : Boolean := False;
   --
   --  Set to non-zero to break out of that many nested loops
   --
   exit_loop : Natural;
   --
   --  Will printing start on a new line
   --
   first_char_flag : Boolean := True;
   --
   --  Initialize the data structures used in the lisp interpreter.
   --
   procedure init;
   --
   --  Function pointers for I/O.  Using these pointers may allow the interpeter
   --  to be run on systems without access to Ada.Text_IO.
   --
   io_put_line : t_put_line;
   io_put      : t_put_line;
   io_new_line : t_newline;
   io_get_line : t_get_line;
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
   --  These procedures print various types of objects.
   --
   procedure print(s : cons_index);
   procedure print(v : value);
   procedure print(s : string_index);
   procedure print(s : symb_index);
   --
   --  This function checks if the lisp read-eval-print loop should exit
   --
   function exit_lisp return Boolean;
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
   --  Temporary symbols are temporary names that may eventually be converted
   --  to regular symbols.
   --
   string_table : array (string_index'First + 1 .. string_index'Last) of fragment;
   --
   --  For debugging, dump the various tables
   --
   procedure dump_cons;
   procedure dump_symbols;
   procedure dump_strings;
   procedure dump(e : element_type);
   procedure dump(s : cons_index);
   procedure dump(s : symb_index);
   --
   --  Local functions and procedures
   --
   --
   --  Replacements for Text_IO to make porting to embedded systems easier.
   --  When on a system without Ada.Text_IO, these will need to be changed to
   --  whatever routines are used.
   --
   procedure put_line(s : String);
   procedure put(s : String);
   procedure new_line;
   procedure Get_Line(Item : out String; Last : out Natural);
   --
   --  Functions for symbols.
   --
   --  If a symbol exists, return it, otherwise create a new symbol.  Returns
   --  false if symbol doesn't exist and can't be created.
   --
   function get_symb(s : out symb_index; n : String) return Boolean;
   function get_symb(s : out symb_index; n : string_index) return Boolean;
   --
   --  Finds a symbol and returns it.  Returns false if symbol can't be found.
   --
--   function find_symb(s : out symb_index; n : String) return Boolean;
--   function find_symb(s : out symb_index; n : string_index) return Boolean;
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
   --  Operations for math and comparisons
   --
   type mathops is (PLUS, MINUS, MUL, DIV);
   type compops is (SYM_EQ, SYM_NE, SYM_LT, SYM_GT);
   --
   --  Function for dispatching the various functions for evaluation.
   --
   function eval_dispatch(s : cons_index) return element_type;


end bbs.lisp;
