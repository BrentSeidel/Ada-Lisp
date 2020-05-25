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
   max_cons : constant Integer := 150;
   max_atom : constant Integer := 100;
   max_symb : constant Integer := 100;
   max_tempym : constant Integer := 50;
   max_string : constant Integer := 200;
   type cons_index is range 0 .. max_cons;
   type atom_index is range 0 .. max_atom;
   type symb_index is range 0 .. max_symb;
   type tempsym_index is range 0 .. max_tempym;
   type string_index is range 0 .. max_string;
   --
   type t_put_line is access procedure(s : String);
   type t_newline is access procedure;
   type t_get_line is access procedure(Item : out String; Last : out Natural);
   --
   --  This indicates what type of an object an element_type is pointing to.  It
   --  can be a cons cell, an atom, or nothing.
   --
   type ptr_type is (CONS_TYPE, ATOM_TYPE, NIL_TYPE);
   --
   --  This indicates what kind of data is in an atom.
   --
   type atom_kind is (ATOM_NIL, ATOM_INTEGER, ATOM_CHARACTER, ATOM_SYMBOL,
                      ATOM_TEMPSYM, ATOM_PARAM, ATOM_STRING, ATOM_LOCAL);
   --
   --  This indicates what kind of data is in a symbol
   --
   type symbol_type is (BUILTIN, LAMBDA, VARIABLE, EMPTY);
   --
   --  Define the contents of records.
   --
   --
   --  An element_type can point to a cons cell, an atom, or can be empty.
   --  This is a pointer to either an atom or a cons cell, used in cons cells.
   --
   type element_type(kind : ptr_type := NIL_TYPE) is
      record
         case kind is
            when CONS_TYPE =>
               ps : cons_index;
            when ATOM_TYPE =>
               pa : atom_index;
            when NIL_TYPE =>
               null;
         end case;
      end record;
   --
   --  An atom can hold various kinds of scalar information.  The ref field is
   --  for holding the reference count used in memory management.
   --
   type atom(kind : atom_kind := ATOM_NIL) is
      record
         ref : Natural;
         case kind is
            when ATOM_NIL =>
               null;
            when ATOM_INTEGER =>
               i : integer;
            when ATOM_CHARACTER =>
               c : Character;
            when ATOM_SYMBOL =>
               sym : symb_index;
            when ATOM_TEMPSYM =>
               tempsym : tempsym_index;
            when ATOM_STRING =>
               str : string_index;
            when ATOM_PARAM =>
               p_name : string_index;
               p_value : element_type;
            when ATOM_LOCAL =>
               l_name : string_index;
               l_value : element_type;
         end case;
      end record;
   --
   --  Type for access to function that implements lisp words.
   --
   type execute_function is access function(e : element_type) return element_type;
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
   atom_table : array (atom_index) of atom;
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
   NIL_ELEM : constant element_type := (Kind => NIL_TYPE);

private
   --
   --  Flag for exiting the REPL
   --
   exit_flag : Boolean := False;
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
--   procedure print(e : element_type; d : Boolean; nl : Boolean);
   procedure print(s : cons_index);
   procedure print(a : atom_index);
   procedure print(s : string_index);
   procedure print(a : atom);
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
         next : Integer range -1 .. Integer(string_index'Last);
         len : Integer range 0..fragment_len;
         str : String (1..fragment_len);
      end record;
   --
   --
   --  Temporary symbols are temporary names that may eventually be converted
   --  to regular symbols.
   --
   tempsym_table : array (tempsym_index) of Integer;
   string_table : array (string_index) of fragment;
   --
   --  For debugging, dump the various tables
   --
   procedure dump_atoms;
   procedure dump_cons;
   procedure dump_symbols;
   procedure dump_tempsym;
   procedure dump_strings;
   procedure dump(e : element_type);
   procedure dump(s : cons_index);
   procedure dump(a : atom_index);
   procedure dump(a : atom);
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
   function find_symb(s : out symb_index; n : String) return Boolean;
   function find_symb(s : out symb_index; n : string_index) return Boolean;
   --
   --  If a temporary symbol exists, return it, otherwise create a new temporary
   --  symbol.  Returns false if symbol doesn't exist and can't be created.
   --
   function get_tempsym(s : out tempsym_index; n : String) return Boolean;
   function get_tempsym(s : out tempsym_index; n : string_index) return Boolean;
   --
   --  Utility functions for manipulating lists
   --
   function atom_to_cons(s : out cons_index; a : atom_index) return Boolean;
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
