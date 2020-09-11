# Ada-Lisp
This is a tiny Lisp interpreter written in Ada.  As such, it lacks many of the features of
a full Common Lisp, but can provide a simple Lisp-like programming environment.

If you find it useful or interesting, drop me a note at brentseidel@gmail.com and
let me know.

Note that this is my first attempt at writing a programming language, so I'm sure
that things could have been done better.  It has been a learning experience and
I'm pleased with how well that it's actually turned out.  This is still under
active development so most things are subject to change, especially the program
internals.

## Interpreter/Compiler
I am calling this an interpreter, though the boundaries are a bit fuzzy.  The input text is
converted into s-expressions that represent the program.  The address for the builtin
operations are stored in the s-expression and are then directly called when evaluated.
It may be considered to be a threaded interpreter (which as nothing to do with programming
threads as a method of concurrent programming).

## Goals
While under initial development, this runs on a host computer (MacOs, in my case), the
goal (achieved) is to get it to run on ARM based embedded systems.  Many of these little
ARM based boards have more computing power than the personal computers that I grew up
with and it seemed like a good idea to provide some sort of interpreter that could be
used to write simple programs directly on the board.  I did briefly toy with the idea
of a tiny BASIC interpreter, but quickly abandoned that idea in favor of Lisp.

The idea is that not only can simple programs be written, but it can also be used to
develop algorithms for accessing the board's hardware.  Once the algorithm development is
finished, they can be translated into Ada and compiled.

## Status

While the only guarentee is that this contains bugs and is missing features, it
is usable.  I've been able to write some small programs in it both on the Mac and
on the Arduino Due.  It is nice to be able to change what the Arduino Due is doing
without having to do a whole compile-load cycle.

### Porting
It now runs on the Arduino Due.  It took a bit of work to remove all dependencies
on Ada libraries that aren't available on the Arduino Due.  Another feature added
was the ability for the host software to add custom lisp commands.  Thus, the
main Arduino Due program can add custom Lisp commands for accessing the Arduino
hardware.

This Lisp interpreter also builds and runs on Windows 10 as well as a Raspberry
PI under Raspberian.

### Supported Data Types
1. Integers are the standard Ada integer type.
2. Strings are variable length and implemented using a linked list.
3. Booleans are either "T" (true) or "NIL" (false).
4. Characters are single ASCII characters (Unicode is not supported).
5. Lists are linked lists of elements that can be of any datatype (even other lists)

### Supported Operations
1. Basic arithmetic
2. Comparisons
3. List operations - CAR and CDR
4. DOWHILE/DOTIMES
5. User defined functions and lambda functions.
6. Local variables.
7. Hardware access.  This is done by allowing the host software to add custom lisp
commands.  This may also be useful for embedding the lisp interpreter in other
applications.
8. Peek and Poke functions for accessing memory.  This is mainly for use on embedded
systems.
9. Logical and bitwise logical operations AND, OR, NOT.

### Non-Supported Features
There are others, but here are the main missing features.  Some of these may eventually
be implemented, others will never be implemented.  The goal is to have a useful
little language, not another port of Common Lisp.
1. Macros.  This is a long term goal.  I would like to implement these, but I need to
figure out how first.
2. Object oriented features.  This will probably never happen.
3. Closures.  This will probably never happen.
4. Packages and similar large program related features.  Remember *Tiny* Lisp.

### Roadmap
The following updates to the language are planned.  They may not be done in the
order shown and other items may be added before some of these.
1. Improved error handling and general code cleanup (ongoing).
2. Convert the BBS.lisp package to a generic with the data structure sizes as
parameters.  This will make it easier to resize things for specific targets.

## Internals

### Memory Management
Since this is intended to run on embedded systems without any memory management, the memory
pools are pre-allocated and sized arrays from which objects can be allocated.  Each object
has a reference count and the object it freed when the number of references reaches 0.  Note
that reference counting is not automatic and has to be done manually.

One advantage that I've discovered about reference counting is that is usually
quickly makes one aware that one has done something wrong.  Two types of errors
are possible - prematurely dereffing an object or not dereffing an object.

The memory manager is probably not thread safe, though this could be an interesting
project.

### Static Tables
Symbols, cons cells, and strings are allocated from statically defined arrays.  This
design decision was made to enable the interpreter to run on systems without any
sort of dynamic memory management.  Basically, I rolled my own.  The size of these
tables are set when compiling the interpreter and can be changed to suit your use.
