with Ada.Strings.Bounded;
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
   --  Since this interpreter is designed to be used on embedded computers with
   --  no operating system and possibly no dynamic memory allocation, The max
   --  sizes for statically allocated data structures is defined here.
   --
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
   --  The actual data types are private.
   --
   --  An atom represents a single element of data.
   --
   type atom is private;
   --
   --  A cons cell contains two element_type pointers that can point to either
   --  an atom or another cons cell.
   --
   type cons is private;
   --
   --  A symbol give a perminant name to a piece of data.  These can be global
   --  variables, user defined functions, or builtin functions.  The builtin
   --  functions are predefined and cannot be changed.
   --
   type symbol is private;
   --
   --  An element_type can point to a cons cell, an atom, or can be empty.
   --
   type element_type is private;
   --
   --  Initialize the data structures used in the lisp interpreter.
   --
   procedure init;
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
   --  These procedures print various types of objects.
   --
   procedure print(e : element_type; d : Boolean; nl : Boolean);
   procedure print(s : cons_index);
   procedure print(a : atom_index);
   procedure print(s : string_index);
   procedure print(a : atom);
   procedure print(s : symb_index);
   --
   --  This function checks if the lisp read-eval-print loop should exit
   --
   function exit_lisp return Boolean;

private
   --
   --  Flag for exiting the REPL
   --
   exit_flag : Boolean := False;
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
   --  This is an enumeration for all the builtin functions.  This list is
   --  expected to grow with time.
   --
   --  The following are currently implemented:
   --    CAR, CDR, PLUS, MINUS, MUL, DIV, SETQ, SYM_EQ, SYM_NE, SYM_LT, SYM_GT,
   --    QUIT_LISP, DUMP, RESET, SYM_TRUE, SYM_IF, DOWHILE, QUOTE, NEWLINE
   --
   --
   type builtins is (CAR, CDR, PRINT, PLUS, MINUS, MUL, DIV, SETQ, SYM_EQ, SYM_NE,
                     SYM_LT, SYM_GT, SYM_AND, SYM_OR, SYM_NOT, QUIT_LISP, DUMP,
                     RESET, SYM_TRUE, SYM_IF, SYM_COND, DEFUN, DOWHILE, DOTIMES,
                     QUOTE, SYM_EVAL, NEWLINE);
   --
   --  Define the contents of records.
   --
   --
   --  An atom can hold various kinds of scalar information.
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
   --  A cons cell contains two element_type pointers.
   --
   type cons is
      record
         ref : Natural;
         car : element_type;
         cdr : element_type;
      end record;
   --
   --  A symbol record contains a name and a type
   --
   type symbol(kind : symbol_type := EMPTY) is
      record
         ref : Natural;
         str : string_index;
         case kind is
            when BUILTIN =>
               i : builtins;
            when LAMBDA =>
               ps : cons_index;
            when VARIABLE =>
               pv : element_type;
            when EMPTY =>
               null;
         end case;
      end record;
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
   --  The main data tables for various kinds of data.
   --
   cons_table : array (cons_index) of cons;
   atom_table : array (atom_index) of atom;
   symb_table : array (symb_index) of symbol;
   --
   --  Temporary symbols are temporary names that may eventually be converted
   --  to regular symbols.
   --
   tempsym_table : array (tempsym_index) of Integer;
   string_table : array (string_index) of fragment;
   --
   --  Some useful constants
   --
   NIL_ELEM : constant element_type := (Kind => NIL_TYPE);
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
   --  Create a symbol for a builtin function.  This is intended to be called
   --  during initialization to identify the builtin operations.  Once created,
   --  these should never be changed.  No value is returned.
   --
   procedure add_builtin(n : String; b : builtins);
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
   --  Procedures for printing error and non-error messages.  Pass in string
   --  representing the function name and the message.  This is intended to
   --  make error messages more consistent.
   --
   procedure error(f : String; m : String);
   procedure msg(f : String; m : String);

end bbs.lisp;
