# Ada-Lisp
This is a tiny Lisp interpreter written in Ada.  As such, it lacks many of the features of
a full Common Lisp, but can provide a simple Lisp-like programming environment.

## Goals
While under initial development, this runs on a host computer (MacOs, in my case), the
goal is to get it to run on Arm based embedded systems.  Many of these little Arm based
boards have more computing power than the personal computers that I grew up with and it
seemed like a good idea to provide some sort of interpreter that could be used to write
simple programs directly on the board.  I did briefly toy with the idea of a tiny BASIC
interpreter, but quickly abandoned that idea in favor of Lisp.

The idea is that not only can simple programs be written, but it can also be used to
develop algorithms for accessing the board's hardware.  Once the algorithm development is
finished, they can be translated into Ada and compiled.

## Status

### Supported Data Types
1. Integers are the standard Ada integer type.
2. Strings are variable length and implemented using a linked list.

### Supported Operations
1. Basic arithmetic
2. Comparisons
3. List operations - CAR and CDR
4. DOWHILE
5. Function definition is under development
6. Macros.  This is a long term goal.  I would like to implemente these, but I need to
figure out how first.
7. Local variables.  I have an idea that might work.
8. Hardware access.  This will be done after the port to target hardware.  Builtin functions
will be defined for accessing various bits of hardware.  These will no doubt have to be
modified when porting between different boards.

## Internals

### Memory Management
Since this is intended to run on embedded systems without any memory management, the memory
pools are pre-allocated and sized arrays from which objects can be allocated.  Each object
has a reference count and the object it freed when the number of references reaches 0.  Note
that reference counting is not automatic and has to be done manually.

The memory manager is probably not thread safe, though this could be an interesting
project.
