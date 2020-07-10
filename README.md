# Ada-Lisp
This is a tiny Lisp interpreter written in Ada.  As such, it lacks many of the features of
a full Common Lisp, but can provide a simple Lisp-like programming environment.

If you find it useful or interesting, drop me a note at brentseidel@gmail.com and
let me know.

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

### Porting
It now runs on the Arduino Due.  It took a bit of work to remove all dependencies
on Ada libraries that aren't available on the Arduino Due.  Another feature added
was the ability for the host software to add custom lisp commands.  Thus, the
main Arduino Due program can add custom lisp commands for accessing the Arduino
hardware.

### Supported Data Types
1. Integers are the standard Ada integer type.
2. Strings are variable length and implemented using a linked list.

### Supported Operations
1. Basic arithmetic
2. Comparisons
3. List operations - CAR and CDR
4. DOWHILE
5. Function definition is under development
6. Hardware access.  This is done by allowing the host software to add custom lisp
commands.  This may also be useful for embedding the lisp interpreter in other
applications.

### Non-Supported Features
There are others, but here are the main missing features.  Some of these may eventually
be implemented, others will never be implemented.  The goal is to have a useful
little language, not another port of Common Lisp.
1. Macros.  This is a long term goal.  I would like to implement these, but I need to
figure out how first.
2. Local variables.  I have an idea that might work.
3. Object oriented features.  This will probably never happen.
4. Closures.  This will probably never happen

## Internals

### Memory Management
Since this is intended to run on embedded systems without any memory management, the memory
pools are pre-allocated and sized arrays from which objects can be allocated.  Each object
has a reference count and the object it freed when the number of references reaches 0.  Note
that reference counting is not automatic and has to be done manually.

The memory manager is probably not thread safe, though this could be an interesting
project.

### Static Tables
Symbols, cons cells, and strings are allocated from statically defined arrays.  This
design decision was made to enable the interpreter to run on systems without any
sort of dynamic memory management.  Basically, I rolled my own.  The size of these
tables are set when compiling the interpreter and can be changed to suit your use.
