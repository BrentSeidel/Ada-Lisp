;
;  Test functions for the Tiny Lisp Interpreter.  This file has been
;  designed so that it can be sourced to the lisp interpreter via a
;  command like:
;
;  ./lisp < test.lisp
;
;  These tests more or less proceed in increasing complexity where later tests
;  depend on items from earlier tests working correctly.  There are some things
;  that are not explicitly tested, such as defun, but are used throughout so
;  failures should be obvious with other things not working properly.
;
;-----------------------------------------
;  Support functions.  Load these first.
;
;  Initialize global counters
;
(setq *PASS-COUNT* 0)
(setq *FAIL-COUNT* 0)
;
;
;  If two values are equal, print pass message, otherwise print fail message
;
(defun verify-equal (expected actual text)
  (if (= actual expected)
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print "PASS: Actual "))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "***FAIL: Actual ")))
  (print  actual ", Expected " expected " " text)
  (terpri))
;
;  Check if a value is true
;
(defun verify-true (act text-msg)
  (verify-equal T act text-msg))
;
;  Check if a value is false
;
(defun verify-false (act text-msg)
  (verify-equal NIL act text-msg))
;
;  Print summary results
;
(defun summary ()
  (print "Test cases passed: " *PASS-COUNT*)
  (terpri)
  (print "Test cases failed: " *FAIL-COUNT*)
  (terpri)
  (print "Total test cases:  " (+ *PASS-COUNT* *FAIL-COUNT*)))
;--------------------------------------------
;  Test cases.  Load and run the desired test.
;
;  Test if and booleans
;
(print "===> Testing booleans")
(terpri)
(defun test-bool ()
  (verify-true (= T T) "T literal is equal to itself")
  (verify-true (= NIL NIL) "NIL literal is equal to itself")
  (verify-false (= NIL T) "NIL and T literals are not equal")
  (verify-true (= T (= 1 1)) "T literal is equal to a true condition")
  (verify-false (= T (= 1 2)) "T literal is not equal to a false condition")
  (verify-true (= NIL (= 1 2)) "NIL literal is equal to a false condition")
  (verify-false (= NIL (= 1 1)) "T literal is not equal to a true condition"))
(test-bool)
(setq test-bool 0)
;
;  Test integer comparisons
;
(print "===> Testing integer comparisons")
(terpri)
(defun test-int-cmp ()
  (verify-true (= 0 0) "0 = 0")
  (verify-false (< 0 0) "not 0 < 0")
  (verify-false (> 0 0) "not 0 > 0")
  (verify-false (/= 0 0) "not 0 /= 0")
  (verify-false (= 2 1) "not 2 = 1")
  (verify-false (< 2 1) "not 2 < 1")
  (verify-true (> 2 1) "2 > 1")
  (verify-true (/= 2 1) "2 /= 1")
  (verify-false (= -1 1) "not -1 = 1")
  (verify-true (< -1 1) "-1 < 1")
  (verify-false (> -1 1) "not -1 > 1")
  (verify-true (/= -1 1) "-1 /= 1")
  (verify-false (= -2 -1) "not -2 = -1")
  (verify-true (< -2 -1) "-2 < -1")
  (verify-false (> -2 -1) "not -2 > -1")
  (verify-true (/= -2 -1) "-2 /= -1"))
(test-int-cmp)
(setq test-int-cmp 0)
;
;  Test string comparisons
;
(print "===> Testing string comparisons")
(terpri)
(defun test-str-cmp ()
  (verify-true (= "A" "A") "A = A")
  (verify-false (< "A" "A") "not A < A")
  (verify-false (> "A" "A") "not A > A")
  (verify-false (/= "A" "A") "not A /= A")
  (verify-false (= "A" "B") "not A = B")
  (verify-true (< "A" "B") "A < B")
  (verify-false (> "A" "B") "not A > B")
  (verify-false (< "C" "B") "C < B")
  (verify-true (> "C" "B") "not C > B")
  (verify-true (/= "A" "B") "A /= B")
  (verify-false (= "A" "AA") "not A = AA")
  (verify-true (< "A" "AA") "A < AA")
  (verify-false (> "A" "AA") "not A > AA")
  (verify-true (/= "A" "AA") "A /= AA"))
(test-str-cmp)
(setq test-str-cmp 0)
;
;  Test boolean comparisons
;
(print "===> Testing boolean comparisons")
(terpri)
(defun test-bool-cmp ()
  (verify-true (= T T) "True is true")
  (verify-false (/= T T) "not T /= T")
  (verify-false (< T T) "not T < T")
  (verify-false (> T T) "not T > T")
  (verify-true (= NIL NIL) "False is false")
  (verify-false (/= NIL NIL) "not NIL /= NIL")
  (verify-false (< NIL NIL) "not NIL < NIL")
  (verify-false (> NIL NIL) "not NIL > NIL")
  (verify-true (> T NIL) "T > NIL")
  (verify-true (< NIL T) "NIL < T")
  (verify-false (= NIL T) "not NIL = T")
  (verify-true (/= T NIL) "T /= NIL")
  (verify-false (> NIL T) "not NIL > T")
  (verify-false (< T NIL) "not T < NIL"))
(test-bool-cmp)
(setq test-bool-cmp 0)
;
;  Test symbol comparisons
;
(print "===> Testing symbol comparisons")
(terpri)
(defun test-sym-comp ()
  (verify-true (= 'print 'print) "Print is equal to itself")
  (verify-false (/= 'print 'print) "not print /= print")
  (verify-false (= 'print 'terpri) "not print = terpri")
  (verify-true (/= 'terpri 'print) "terpri is not print"))
(test-sym-comp)
(setq test-sym-comp 0)
;
;  Test basic math functions
;
(print "===> Testing basic math operations")
(terpri)
(defun test-basic-math ()
  (verify-equal 6 (+ 1 2 3) "Add positive")
  (verify-equal 0 (+ 1 2 -3) "Add mixed")
  (verify-equal -6 (+ -1 -2 -3) "Add negative")
  (verify-equal 24 (* 2 3 4) "Mul positive")
  (verify-equal -24 (* -2 3 4) "Mul mixed")
  (verify-equal 24 (* -6 -4) "Mul negative")
  (verify-equal -4 (- 1 2 3) "Sub result negative")
  (verify-equal 3 (- 4 1) "Sub positive")
  (verify-equal 1 (- -2 -3) "Sub negative")
  (verify-equal -5 (- -2 3) "Sub mixed")
  (verify-equal 2 (/ 24 3 4) "Div positive")
  (verify-equal -8 (/ -24 3) "Div mixed")
  (verify-equal 8 (/ -24 -3) "Div negative")
  (verify-equal 4 (/ 24 5) "Div inexact"))
;
(test-basic-math)
(setq test-basic-math 0)
;
;  Test global variables with setq
;
(print "===> Testing globals")
(terpri)
(defun test-global ()
  (setq **GLOB** 1)
  (verify-equal 1 **GLOB** "Set global variable")
  (setq **GLOB** (+ 1 **GLOB**))
  (verify-equal 2 **GLOB** "Increment global")
  (setq **GLOB** "Hello")
  (verify-equal "Hello" **GLOB** "Set global to string"))
(test-global)
(setq test-global 0)
;
;  Test local variables with setq
;
(print "===> Testing let")
(terpri)
(defun test-let ()
  (let ((a (+ 1 2)) (b 4) (c 0))
    (verify-equal 3 a "A is 3")
    (verify-equal 4 b "B is 4")
    (verify-equal 0 c "C is 0")
    (setq a (+ 1 a))
    (verify-equal 4 a "A is 4")
    (setq b -10)
    (verify-equal -10 b  "B is -10")
    (setq c (+ a 2))
    (verify-equal 6 c "C is 6")
    (let ((a 1))
      (verify-equal 1 a "A is 1")
      (setq a 20)
      (verify-equal 20 a "A is 20"))
    (verify-equal 4 a "A is 4")))
(test-let)
(setq test-let 0)
;
;  Test dowhile operation
;
(print "===> Testing dowhile")
(terpri)
(defun test-dowhile ()
  (let ((count 0) (accum 0))
    (dowhile (< count 10)
      (setq count (+ count 1))
      (setq accum (+ accum count))
      (print "Counter is " count ", Accumulator is " accum)
      (terpri))
    (verify-equal 10 count "Count is 10")
    (verify-equal 55 accum "Accumulator is 55")))
(test-dowhile)
(setq test-dowhile 0)
;
;  Test dotimes operation
;
(print "===> Testing dotimes")
(terpri)
(defun test-dotimes ()
  (let ((accum 0) result)
    (setq result (dotimes (n 10 20)
      (print "Count is " n)
      (terpri)
      (setq accum (+ accum n))))
    (verify-equal 20 result "Result returned")
    (verify-equal 45 accum "Accumulator is 45")))
(test-dotimes)
(setq test-dotimes 0)
;
;  Test progn and return
;
(print "===> Testing block related operations")
(terpri)
(defun test-block ()
  (let ((accum 0) (count 0) result)
    (setq result (dowhile (< count 10)
      (print "Count is " count)
      (terpri)
      (if (= count 5)
        (progn
          (verify-equal 5 count "Loop at 5")
          (return 50)
          (verify-equal 1 2 "Return operation failed")))
       (setq count (+ 1 count))))
    (verify-equal 50 result "Checking result from return")
    (verify-equal 5 count "Loop exited at count = 5")
    (setq count T)
    (setq accum 0)
    (dowhile count (if (= accum 5) (setq count NIL)) (setq accum (+ accum 1)))
    (verify-equal 6 accum "Accumulator is 6")
    (verify-equal NIL count "Count is set to NIL")
))
(test-block)
(setq test-block 0)
;
;  Test Hex number representation
;
(print "===> Testing hexidecimal numbers")
(terpri)
(defun test-hex ()
  (verify-equal #x0 0 "Digit 0")
  (verify-equal #x1 1 "Digit 1")
  (verify-equal #x2 2 "Digit 2")
  (verify-equal #x3 3 "Digit 3")
  (verify-equal #x4 4 "Digit 4")
  (verify-equal #x5 5 "Digit 5")
  (verify-equal #x6 6 "Digit 6")
  (verify-equal #x7 7 "Digit 7")
  (verify-equal #x8 8 "Digit 8")
  (verify-equal #x9 9 "Digit 9")
  (verify-equal #xA 10 "Digit A")
  (verify-equal #xB 11 "Digit B")
  (verify-equal #xC 12 "Digit C")
  (verify-equal #xD 13 "Digit D")
  (verify-equal #xE 14 "Digit E")
  (verify-equal #xF 15 "Digit F")
  (verify-equal #xFF 255 "Hex FF")
  (verify-equal #x1000 4096 "Hex 1000")
)
(test-hex)
(setq test-hex 0)
;
;  Test logical operations
;
(print "===> Testing logic operations")
(terpri)
(defun test-logic ()
  (verify-equal NIL (not T) "T -> NIL")
  (verify-equal T (not NIL) "NIL -> T")
  (verify-equal #x0000FFFF (not #xFFFF0000) "#xFFFF0000 -> #x0000FFFF")
  (verify-equal #xA5A5A5A5 (not #x5A5A5A5A) "#x5A5A5A5A -> #xA5A5A5A5")
  (verify-equal NIL (and NIL NIL) "NIL and NIL -> NIL")
  (verify-equal NIL (and NIL T) "NIL and T -> NIL")
  (verify-equal NIL (and T NIL) "T and NIL -> NIL")
  (verify-equal T (and T T) "T and T -> T")
  (verify-equal NIL (or NIL NIL) "NIL or NIL -> NIL")
  (verify-equal T (or NIL T) "NIL or T -> T")
  (verify-equal T (or T NIL) "T or NIL -> T")
  (verify-equal T (or T T) "T or T -> T")
  (verify-equal 0 (and 15 4 3) "Produces zero")
  (verify-equal 15 (and #xF0F #xF01F)  "Produces 15")
  (verify-equal 7 (or 1 2 4) "Produces 7")
  (verify-equal #xF0F0 (or #xF000 #xF0) "Produces #xF0F0")
  (verify-equal T (not (and T NIL)) "T nand NIL -> T"))
(test-logic)
(setq test-logic 0)
;
;  Test character data type and operations.  Some of these tests assume
;  ASCII encoding.
;
(print "===> Testing characters")
(terpri)
(defun test-char ()
  (verify-true (= #\A #\A) "A = A")
  (verify-false (= #\A #\B) "A /= B")
  (verify-true (< #\A #\B) "A < B")
  (verify-false (< #\A #\A) "A /< A")
  (verify-true (> #\B #\A) "B > A")
  (verify-false (> #\B #\B) "B /> B")
  (verify-true (/= #\A #\N) "A /= N")
  (verify-false (/= #\A #\A) "A = A")
  (verify-equal (char-code #\A) 65 "A has character code 65")
  (verify-equal (code-char 66) #\B "Character code 66 is B")
  (verify-equal (char-downcase #\A) #\a "Lower case A is a")
  (verify-equal (char-upcase #\a) #\A "Upper case a is A")
  (verify-equal (char-code #\space) 32 "#\space represents a space")
  (verify-equal (char-code #\newline) 10 "#\newline represents a line feed")
  (verify-equal (char-code #\tab) 9 "#\tab represents a tab")
  (verify-equal (char-code #\page) 12 "#\page represents a form feed")
  (verify-equal (char-code #\rubout) 127 "#\rubout represents a delete")
  (verify-equal (char-code #\linefeed) 10 "#\linefeed represents a line feed")
  (verify-equal (char-code #\return) 13 "#\return represents a carriage return")
  (verify-equal (char-code #\backspace) 8 "#\backspace represents a backspace")
  (verify-true (errorp #\bad) "#\bad is an error"))
(test-char)
(setq test-char 0)
;
;  Test string operations
;
(print "===> Testing strings")
(terpri)
(defun test-str ()
  (verify-equal 5 (length "Hello") "Length of hello")
  (verify-equal 26 (length "abcdefghijklmnopqrstuvwxyz") "Length of alphabet")
  (verify-equal 4 (length (list 1 2 3 4)) "Length of list")
  (verify-equal 1 (length 1) "Length of integer")
  (verify-equal 0 (length ()) "Length of empty list")
  (verify-equal #\z (char "abcdefghijklmnopqrstuvwxyz" 25) "Last letter of alphabet")
  (verify-equal #\a (char "abcdefghijklmnopqrstuvwxyz" 0) "First letter of alphabet")
  (verify-equal 42 (parse-integer "42") "The meaning of life")
  (verify-equal -100 (parse-integer "-100") "Negative number")
  (verify-equal 0 (parse-integer "hello") "Not a number")
  (verify-equal "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123"
    (string-upcase "abcdefghijklmnopqrstuvwxyz0123") "Upper case text")
  (verify-equal "abcdefghijklmnopqrstuvwxyz0123"
    (string-downcase "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123") "Lower case text")
  (verify-equal "-bye" (subseq "Good-bye" 4) "Subsequence with default end")
  (verify-equal "d-b" (subseq "Good-bye" 3 6) "Subsequence with specified end")
  (verify-equal "fairly long string t"
    (subseq "This is a fairly long string that should span multiple fragments" 10 30)
    "Subsequence of a long string"))
(test-str)
(setq test-str 0)
;
;  Test predicates
;
(print "===> Testing predicates")
(terpri)
(defun test-pred ()
  (verify-false (arrayp (1 3 4)) "arrayp is always false")
  (verify-false (bit-vector-p (1 2 3)) "bit-vector-p is always false")
  (verify-false (complexp 1) "complexp is always false")
  (verify-false (floatp (/ 1 3)) "floatp is always false")
  (verify-false (vectorp (1 2 3)) "vectorp is always false")
  (verify-false (rationalp (/ 2 5)) "rationalp is always false")
  (verify-false (realp (/ 5 2)) "realp is always false")
  (verify-false (simple-vector-p (1 2 3)) "simple-vector-p is always false")
  (verify-false (simple-bit-vector-p #x0F0F0F0F) "simple-bit-vector-p is always false")
  (verify-false (packagep "main") "packagep is always false")
  (verify-false (vectorp (1 2 3)) "vectorp is always false")
  (verify-true (atomp 1) "atomp is true for atoms")
  (verify-false (atomp (1)) "atomp is false for non-atoms")
  (verify-true (characterp #\A) "characterp is true for characters")
  (verify-false (characterp 1) "characterp is false for non-characters")
  (verify-true (compiled-function-p print) "compiled-function-p is true for builtins")
  (verify-false (compiled-function-p test-pred) "compiled-function-p is false for non-builtins")
  (verify-true (consp (1 2 3)) "consp is true for lists")
  (verify-false (consp 1) "consp is false for non-lists")
  (verify-false (errorp 1) "errorp is false for normal conditions")
  (verify-true (errorp (+ 1 "a")) "errorp is true for error conditions")
  (verify-true (functionp test-pred) "functionp is true for user defined functions")
  (verify-true (functionp functionp) "functionp is true for builtin functions")
  (verify-true (functionp setq) "functionp is true for special functions")
  (verify-true (functionp (lambda (a b) (+ a b))) "functionp is true for lambda functions")
  (verify-false (functionp 1) "functionp is false for non-functions")
  (verify-true (integerp 3) "integerp is true for integers")
  (verify-false (integerp #\A) "integerp is false for non-integers")
  (verify-true (listp (2 4 6)) "listp is true for lists")
  (verify-false (listp "Hello") "listp is false for non-lists")
  (verify-true (numberp 10) "numberp is true for numbers")
  (verify-false (numberp #\B) "numberp is false for non-numbers")
  (verify-true (null ()) "null is true for nulls")
  (verify-false (null 1) "null is false for non-nulls")
  (verify-true (simple-string-p "Hello") "simple-string-p is true for strings")
  (verify-false (simple-string-p 2) "simple-string-p is false for non-strings")
  (verify-true (stringp "Hello") "stringp is true for strings")
  (verify-false (stringp #\C) "stringp is false for non-strings")
  (verify-true (symbolp car) "symbolp is true for symbols")
  (verify-false (symbolp #\@) "symbolp is false for non-symbol"))
(test-pred)
(setq test-pred 0)
;
;  Test coerce and quoted symbols
;
(print "===> Testing coercion")
(terpri)
(defun test-coerce ()
  (verify-equal 1 (coerce t 'integer) "Boolean to integer")
  (verify-equal 0 (coerce nil 'integer) "Boolean to integer")
  (verify-equal 123 (coerce 123 'integer) "Integer to integer")
  (verify-equal "T" (coerce t 'string) "Boolean to string")
  (verify-equal "NIL" (coerce nil 'string) "Boolean to string")
  (verify-equal 1 (coerce t 'integer) "Boolean to integer")
  (verify-true (errorp (coerce "A" 'integer)) "String to integer is error")
  (verify-equal NIL (coerce nil 'boolean) "Boolean to boolean")
  (verify-true (errorp (coerce 1 'string)) "Integer to string is error")
  (verify-equal "A" (coerce #\A 'string) "Character to string")
  (verify-equal "@" (coerce #\@ 'string) "Character to string")
  (verify-equal "Hello" (coerce "Hello" 'string) "String to string")
  (verify-equal #\A (coerce #\A 'character) "Character to character")
  (verify-true (errorp (coerce 1 'character)) "Integer to character is error")
  (verify-true (errorp (coerce #\A 'boolean)) "Characeter to boolean is error")
  (verify-equal T (coerce 1 'boolean) "Integer to boolean")
  (verify-equal NIL (coerce 0 'boolean) "Integer to boolean")
  (verify-true (errorp (coerce)) "Called with no parameters")
  (verify-true (errorp (coerce 1)) "Called with only one parameter")
  (verify-true (errorp (coerce #\error 'string)) "First parameter is an error")
  (verify-true (errorp (coerce 1 1)) "Second parameter is not a symbol")
  (verify-true (errorp (coerce 1 'print)) "Second parameter does not represent a type")
  (verify-true (errorp (coerce (1 2 3) 'string)) "First parameter is a list")
  (verify-true (errorp (coerce 1 #\error)) "Second parameter is an error")
  (verify-true (errorp (coerce 1 (1 2 3))) "Second parameter is a list"))
(test-coerce)
(setq test-coerce 0)
;
;  Test concatenation of strings.  Lists can't be compared so
;  they aren't tested.
;
(print "==> Testing concatenation")
(terpri)
(defun test-concatenate ()
  (verify-equal "Hello world!" (concatenate 'string "Hello" " " "world" "!") "String concatenation")
  (verify-false (errorp (concatenate 'list (1 2 3) (4 5 6))) "Concatenate lists")
  (verify-equal 6 (length (concatenate 'list (1 2 3) (4 5 6))) "Concatenate lists")
  (verify-true (errorp (concatenate)) "Concatenate with no parameters")
  (verify-true (errorp (concatenate 1 "A" "B")) "Concatenate with first parameter not symbol")
  (verify-true (errorp (concatenate #\error "A" "B")) "Concatenate with first parameter error")
  (verify-true (errorp (concatenate (1 2 3) (4 5 6))) "Concatenate with first parameter a list")
  (verify-true (errorp (concatenate 'integer 1 1)) "Can't concatenate integers")
  (verify-true (errorp (concatenate 'string 1 2 3)) "Integers aren't strings")
  (verify-true (errorp (concatenate 'string "A" (1 2 3))) "Mixed concatenation")
  (verify-true (errorp (concatenate 'string)) "Concatenate a no strings")
  (verify-true (errorp (concatenate 'list)) "Concatenate a no lists")
  (verify-true (errorp (concatenate 'string #\error)) "Error as second parameter")
  (verify-true (errorp (concatenate 'list #\error)) "Error as second parameter")
  )
(test-concatenate)
(setq test-concatenate 0)
;
;  Test lambda
;
(print "==> Testing Lambda")
(terpri)
(defun test-lambda (a b c d)
  (verify-equal a (b c d) "Testing lambda"))
(test-lambda 3 (lambda (a1 a2) (+ a1 a2)) 1 2)
(test-lambda 6 (lambda (a1 a2) (* a1 a2)) 2 3)
(test-lambda T (lambda (a1 a2) (< a1 a2)) #\A #\B)
(test-lambda T (lambda (a1 a2)(= a1 (* a2 (/ a1 a2)))) 6 2)
(setq test-lambda 0)
;
;  Test miscellaneous items.  These have out of band effects, so the
;  checks mainly test that they properly recognize valid and invalid
;  parameters.
;
(print "===> Testing miscellaneous operations")
(terpri)
(defun test-misc ()
  (verify-equal NIL (msg T) "Message on returns NIL")
  (verify-equal NIL (msg NIL) "Message off returns NIL")
  (verify-true (errorp (msg)) "No parameter to message")
  (verify-true (errorp (msg "Hello")) "String parameter to message")
  (verify-equal NIL (dump) "Dump returns NIL")
  (verify-equal NIL (sleep 1) "Sleep returns NIL")
  (verify-true (errorp (sleep)) "No parameter to sleep")
  (verify-true (errorp (sleep "Wake")) "String parameter to sleep"))
(test-misc)
(setq test-misc 0)
;
;  Test I/O operations.  These have out of band effects, so the
;  checks mainly test that proper values are returned and that errors
;  don't occur for valid inputs.
;
(print "===> Testing I/O operations")
(terpri)
(defun test-io ()
  (verify-equal NIL (print 1) "Print returns NIL")
  (verify-equal NIL (print #\1) "Print returns NIL")
  (verify-equal NIL (print "1") "Print returns NIL")
  (verify-equal NIL (print 'print) "Print returns NIL")
  (verify-equal NIL (print (1 2 3)) "Print returns NIL")
  (verify-equal NIL (terpri) "Terpri returns NIL")
  (verify-equal NIL (fresh-line) "Fresh-line returns NIL")
  (verify-equal NIL (progn (print "Hello") (fresh-line)) "Fresh-line returns NIL")
  (verify-equal "Testing a longer line that should be split across fragments."
    (read-line) "Text read from read-line"))
(test-io)
Testing a longer line that should be split across fragments.
(setq test-io 0)
;
;  Test memory operations.  These read and write arbitrary memory, which
;  can have unpleasant side effects.  Only the error cases are tested.
;
(print "===> Testing memory operation errors")
(terpri)
(defun test-mem-err ()
  (verify-true (errorp (peek8)) "No parameter to peek8")
  (verify-true (errorp (peek16)) "No parameter to peek16")
  (verify-true (errorp (peek32)) "No parameter to peek32")
  (verify-true (errorp (peek8 "Hello")) "String parameter to peek8")
  (verify-true (errorp (peek16 "Hello")) "String parameter to peek16")
  (verify-true (errorp (peek32 "Hello")) "String parameter to peek32")
  (verify-true (errorp (poke8)) "No parameter to poke8")
  (verify-true (errorp (poke16)) "No parameter to poke16")
  (verify-true (errorp (poke32)) "No parameter to poke32")
  (verify-true (errorp (poke8 1)) "One parameter to poke8")
  (verify-true (errorp (poke16 1)) "One parameter to poke16")
  (verify-true (errorp (poke32 1)) "One parameter to poke32")
  (verify-true (errorp (poke8 "Hello" 1)) "String parameter to poke8")
  (verify-true (errorp (poke16 "Hello" 1)) "String parameter to poke16")
  (verify-true (errorp (poke32 "Hello" 1)) "String parameter to poke32")
  (verify-true (errorp (poke8 1 "Hello")) "String parameter to poke8")
  (verify-true (errorp (poke16 1 "Hello")) "String parameter to poke16")
  (verify-true (errorp (poke32 1 "Hello")) "String parameter to poke32"))
(test-mem-err)
(setq test-mem-err 0)
;
;  Test list operations.  Since list comparisons aren't currently suppored,
;  these aren't quite as nice as they might be.
;
(print "===> Testing list operations")
(terpri)
(defun test-list ()
  (verify-equal 3 (length (1 2 3)) "Length of a raw list")
  (verify-equal 4 (length (quote "a" "B" 3 4)) "Length of a quoted list")
  (verify-equal 5 (car (5 1 2 3 4)) "CAR of a list")
  (verify-equal 4 (length (cdr (5 6 7 8 9))) "Length of CDR")
  (verify-equal 2 (length (cons (2 3 4) 1)) "CONS of elements")
  (verify-equal 4 (length (cons 1 (2 3 4))) "CONS of elements")
  (verify-equal 1 (car (cons 1 (2 3 4))) "CAR of a CONS")
  (verify-equal 2 (car (cdr (1 2 3 4))) "CAR of a CDR of a list")
  (verify-equal 5 (car 5) "CAR of a single value")
  (verify-equal NIL (car) "CAR of nothing is nothing")
  (verify-equal NIL (cdr) "CDR of nothing is nothing")
  (varify-equal NIL (cdr 1) "CDR of a value is nothing")
  (verify-true (errorp (cons)) "No parameter to CONS")
  (verify-true (errorp (cons #\error 1)) "CONS error in first parameter")
  (verify-true (errorp (cons 1 #\error)) "CONS error in second parameter")
  (verify-true (errorp (list #\error 1 2 3)) "LIST error in first parameter")
  (verify-true (errorp (list 1 2 #\error 3)) "LIST error in later parameter")
  (verify-equal NIL (list) "LIST with no parameters is nothing")
)
(test-list)
(setq test-list 0)
;
;  Test error conditions.  There are lots so they will be broken down into smaller
;  groups.
;
(print "==> Testing comparison error conditions")
(terpri)
(defun test-cmp-err ()
  (verify-true (errorp (=)) "No parameters to equal")
  (verify-true (errorp (= 1)) "One parameters to equal")
  (verify-true (errorp (= 1 "A")) "Mismatched parameters to equal")
  (verify-true (errorp (/=)) "No parameters to not-equal")
  (verify-true (errorp (/= 1)) "One parameters to not-equal")
  (verify-true (errorp (/= 1 "A")) "Mismatched parameters to not-equal")
  (verify-true (errorp (<)) "No parameters to less-than")
  (verify-true (errorp (< 1)) "One parameters to less-than")
  (verify-true (errorp (< 1 "A")) "Mismatched parameters to less-than")
  (verify-true (errorp (>)) "No parameters to greater-than")
  (verify-true (errorp (> 1)) "One parameters to greater-than")
  (verify-true (errorp (> 1 "A")) "Mismatched parameters to greater-than")
  (verify-true (errorp (> 'print 'terpri)) "Symbols are not ordered")
  (verify-true (errorp (< 'print 'terpri)) "Symbols are not ordered")
  (verify-true (errorp (= (msg) 1)) "Error in first parameter")
  (verify-true (errorp (/= 1 (msg))) "Error in second parameter")
  (verify-true (errorp (< (1 2 3) 1)) "List in first parameter")
  (verify-true (errorp (> 1 (1 2 3))) "List in second parameter")
  (verify-true (errorp (if (msg) 1 2)) "Error in if condition")
  (verify-true (errorp (if T (msg) 2)) "Error in if true branch")
  (verify-true (errorp (if NIL 1 (msg))) "Error in if false branch")
  (verify-true (errorp (if)) "No parameters to if")
)
(test-cmp-err)
(setq test-cmp-err 0)
;
(print "==> Testing math error conditions")
(terpri)
(defun test-math-err ()
  (verify-true (errorp (+)) "No parameters to addition")
  (verify-true (errorp (+ 1 "A")) "Mismatched parameters to addition")
  (verify-true (errorp (+ 1 (msg))) "Error parameter to addition")
  (verify-true (errorp (+ 1 (1 2 3))) "Error parameter to addition")
  (verify-true (errorp (-)) "No parameters to subtraction")
  (verify-true (errorp (- 1 "A")) "Mismatched parameters to subtraction")
  (verify-true (errorp (- 1 (msg))) "Error parameter to subtraction")
  (verify-true (errorp (- 1 (1 2 3))) "Error parameter to subtraction")
  (verify-true (errorp (/)) "No parameters to division")
  (verify-true (errorp (/ 1 "A")) "Mismatched parameters to division")
  (verify-true (errorp (/ 1 (msg))) "Error parameter to division")
  (verify-true (errorp (/ 1 (1 2 3))) "Error parameter to division")
  (verify-true (errorp (*)) "No parameters to multiplication")
  (verify-true (errorp (* "A" 1)) "Mismatched parameters to multiplication")
  (verify-true (errorp (* (msg) 1)) "Error parameter to multiplication")
  (verify-true (errorp (* (1 2 3) 1)) "Error parameter to multiplication"))
(test-math-err)
(setq test-math-err 0)
;
(print "==> Testing logical operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-true (errorp (not)) "No parameter to NOT")
  (verify-true (errorp (not "A")) "Invalid parameter type to NOT")
  (verify-true (errorp (not (msg))) "Error parameter type to NOT")
  (verify-true (errorp (not (1 2 3))) "List parameter type to NOT")
  (verify-true (errorp (and)) "No parameter to AND")
  (verify-true (errorp (and "A")) "Invalid parameter type to AND")
  (verify-true (errorp (and T 1)) "Mixed parameter types to AND")
  (verify-true (errorp (and T (msg))) "Error parameter type to AND")
  (verify-true (errorp (and T (1 2 3))) "List parameter type to AND")
  (verify-true (errorp (or)) "No parameter to OR")
  (verify-true (errorp (or "A")) "Invalid parameter type to OR")
  (verify-true (errorp (or 1 NIL)) "Mixed parameter types to OR")
  (verify-true (errorp (or NIL (msg))) "Error parameter type to OR")
  (verify-true (errorp (or 0 (1 2 3))) "List parameter type to OR"))
(test-log-err)
(setq test-log-err 0)
;
(print "==> Testing character operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-true (errorp (char-code)) "No parameter to char-code")
  (verify-true (errorp (char-code "A")) "Invalid parameter type to char-code")
  (verify-true (errorp (char-code (1 2 3))) "List parameter type to char-code")
  (verify-true (errorp (char-code (msg))) "Error parameter type to char-code")
  (verify-true (errorp (code-char)) "No parameter to code-char")
  (verify-true (errorp (code-char "A")) "Invalid parameter type to code-char")
  (verify-true (errorp (code-char (1 2 3))) "List parameter type to code-char")
  (verify-true (errorp (code-char (msg))) "Error parameter type to code-char")
  (verify-true (errorp (code-char 256)) "Out of range parameter to code-char")
  (verify-true (errorp (char-downcase)) "No parameter to char-downcase")
  (verify-true (errorp (char-downcase "A")) "Invalid parameter type to char-downcase")
  (verify-true (errorp (char-downcase (1 2 3))) "List provided to char-downcase")
  (verify-true (errorp (char-downcase (msg))) "Error provided to char-downcase")
  (verify-true (errorp (char-upcase)) "No parameter to char-upcase")
  (verify-true (errorp (char-upcase "A")) "Invalid parameter type to char-upcase")
  (verify-true (errorp (char-upcase (1 2 3))) "List provided to char-upcase")
  (verify-true (errorp (char-upcase (msg))) "Error provided to char-upcase"))
(test-log-err)
(setq test-log-err 0)
;
(print "==> Testing string operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-true (errorp (char)) "No parameter to char")
  (verify-true (errorp (char "A" \#A)) "Invalid parameter type to char")
  (verify-true (errorp (char 1 "A")) "Invalid parameter type to char")
  (verify-true (errorp (parse-integer)) "No parameter to parse-integer")
  (verify-true (errorp (string-downcase)) "No parameter to string-downcase")
  (verify-true (errorp (string-downcase 1)) "Invalid parameter type to string-downcase")
  (verify-true (errorp (string-upcase)) "No parameter to string-upcase")
  (verify-true (errorp (string-upcase \#b)) "Invalid parameter type to string-upcase")
  (verify-true (errorp (subseq)) "No parameter type to subseq")
  (verify-true (errorp (subseq 1 2 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "A" 2 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "ABC" #\a 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "ABC" 1 #\b)) "Invalid parameter type to subseq"))
(test-log-err)
(setq test-log-err 0)
;
(print "===> Testing predicate error conditions")
(terpri)
(defun test-pred-err ()
;
;  Test cases commented out because as these types are not supported in
;  Tiny-Lisp, the predicates for them always return false even with no
;  parameters.
;
;  (verify-true (errorp (arrayp)) "No parameter to arrayp")
;  (verify-true (errorp (bit-vector-p)) "No parameter to bit-vector-p")
;  (verify-true (errorp (complexp)) "No parameter to complexp")
;  (verify-true (errorp (floatp)) "No parameter to floatp")
;  (verify-true (errorp (vectorp)) "No parameter to vectorp")
;  (verify-true (errorp (rationalp)) "No parameter to rationalp")
;  (verify-true (errorp (realp)) "No parameter to realp")
;  (verify-true (errorp (simple-vector-p)) "No parameter to simple-vector-p")
;  (verify-true (errorp (simple-bit-vector-p)) "No parameter to simple-bit-vector-p")
;  (verify-true (errorp (packagep)) "No parameter to packagep")
;  (verify-true (errorp (vectorp)) "No parameter to vectorp")
  (verify-true (errorp (atomp)) "No parameter to atomp")
  (verify-true (errorp (characterp)) "No parameter to characterp")
  (verify-true (errorp (compiled-function-p)) "No parameter to compiled-function-p")
  (verify-true (errorp (consp)) "No parameter to consp")
  (verify-true (errorp (errorp)) "No parameter to errorp")
  (verify-true (errorp (functionp)) "No parameter to functionp")
  (verify-true (errorp (integerp)) "No parameter to integerp")
  (verify-true (errorp (listp)) "No parameter to listp")
  (verify-true (errorp (numberp)) "No parameter to numberp")
  (verify-true (errorp (null)) "No parameter to null")
  (verify-true (errorp (simple-string-p)) "No parameter to simple-string-p")
  (verify-true (errorp (stringp)) "No parameter to stringp")
  (verify-true (errorp (symbolp)) "No parameter to symbolp"))
(test-pred-err)
(setq test-pred-err 0)
;
(print "===> Testing coercion errors")
(terpri)
(defun test-coerce-err ()
  (verify-true (errorp (coerce)) "No paramaters")
  (verify-true (errorp (coerce nil)) "One parameter")
  (verify-true (errorp (coerce t 'print)) "Wrong coercion")
  (verify-true (errorp (coerce nil 'character)) "Invalid coercion"))
(test-coerce-err)
(setq test-coerce-err 0)
;
(print "==> Testing concatenation errors")
(terpri)
(defun test-concatenate-err ()
  (verify-true (errorp (concatenate)) "No parameters")
  (verify-true (errorp (concatenate 'string)) "One parameter")
  (verify-true (errorp (concatenate 'print "Hello " "World")) "Invalid type")
  (verify-true (errorp (concatenate 'list (1 2 3) "world")) "Wrong type")
  (verify-true (errorp (concatenate 'string (1 2 3) "world")) "Wrong type")
  (verify-true (errorp (concatenate 'string "Hello " 1)) "Wrong type"))
(test-concatenate-err)
(setq test-concatenate-err 0)
;
;  Testing function errors is a bit difficult because defining functions
;  within functions is not supported.
;
(print "===> Testing function errors")
(terpri)
(defun test1 (a) (print (+ a 1)))
(verify-true (errorp (test1)) "Function called with wrong number of parameters")
(verify-true (errorp (defun)) "No parameters to defun")
(verify-true (errorp (defun print)) "Redefine a builtin")
(verify-true (errorp (defun 3)) "Function name must be a symbol")
(verify-true (errorp (defun test1)) "Only function name provided")
(verify-true (errorp (defun test1 1 (print (+ 1 1)))) "Parameter list is not a list")
(verify-true (errorp (defun test1 (1) (print (+ 1 1)))) "Parameter is a value")
(verify-true (errorp (defun test1 (print) (print (+ 1 1)))) "Parameter is a builtin")
(verify-true (errorp (defun test1 ((1)) (print (+ 1 1)))) "Parameter is a list")
(verify-true (errorp (lambda)) "Lambda with no parameters")
(verify-true (errorp (lambda 1 (print (+ 1 1)))) "Lambda parameters must be a list")
(verify-true (errorp (lambda (1) (print (+ 1 1)))) "lambda with value parameter")
(verify-true (errorp (lambda (print) (print (+ 1 1)))) "lambda with builtin symbol parameter")
(verify-true (errorp (lambda ((1)) (print (+ 1 1)))) "lambda with list parameter")
;
(print "===> Testing DOWHILE errors")
(terpri)
(defun check-err (b) (if (< b 3) b #\error))
(defun test-dowhile-err ()
  (verify-true (errorp (dowhile)) "No parameters to dowhile")
  (verify-true (errorp (dowhile #\error (print "Hello"))) "Error in condition")
  (let ((a 0))
    (verify-true (errorp (dowhile (< a 3) (setq a (+ a 1)) (+ 1 #\error))) "Error in block"))
  (let ((a 0))
    (verify-true (errorp (dowhile (check-err a) (setq a (+ a 1)))) "Error later in condition"))
)
(test-dowhile-err)
(setq test-dowhile-err 0)
(setq check-err 0)
;
(print "===> Testing DOTIMES errors")
(terpri)
(defun test-dotimes-err ()
  (verify-true (errorp (dotimes)) "No parameters to dotimes")
  (verify-true (errorp (dotimes 1 1)) "Limits need to be a list")
  (verify-true (errorp (dotimes ((1 2 3) 1 2) (print "Hi"))) "Loop variable is a list")
  (verify-true (errorp (dotimes ('print 1 2) (print "Hi"))) "Loop variable is a builtin")
  (verify-true (errorp (dotimes (1) 1)) "No limit variable")
  (verify-true (errorp (dotimes (a #\error) 1)) "Error in limits")
  (verify-true (errorp (dotimes (a -1) 1)) "Limit is negative")
  (verify-true (errorp (dotimes (a #\A) 1)) "Limit is not integer")
  (verify-true (errorp (dotimes (a 5) (+ 1 #\error))) "Error in loop body")
)
(test-dotimes-err)
(setq test-dotimes-err 0)
;
(print "===> Testing string function errors")
(terpri)
(defun test-str-err ()
  (verify-true (errorp (length)) "No parameters to length")
  (verify-true (errorp (char 1 1)) "Char first parameter not string")
  (verify-true (errorp (char (1 2 3) 1)) "Char first parameter is a list")
  (verify-true (errorp (char "Hello" #\A)) "Char second parameter not integer")
  (verify-true (errorp (char "Hello" -1)) "Char index negative")
  (verify-true (errorp (char "Hello" 100)) "Char index too big")
  (verify-true (errorp (parse-integer)) "No parameter to parse-integer")
  (verify-true (errorp (parse-integer 1)) "Integer parameter to parse-integer")
  (verify-true (errorp (parse-integer (1 2 3))) "List parameter to parse-integer")
  (verify-true (errorp (subseq)) "No parameters to subseq")
  (verify-true (errorp (subseq #\error 2 2)) "Error in first parameter to subseq")
  (verify-true (errorp (subseq 2 2 2)) "First parameter of subseq is not string")
  (verify-true (errorp (subseq (1 2 3) 2 2)) "First parameter of subseq is a list")
  (verify-true (errorp (subseq "Hello" #\error 2)) "Error in second parameter to subseq")
  (verify-true (errorp (subseq "Hello" #\A 2)) "Second parameter of subseq is not integer")
  (verify-true (errorp (subseq "Hello" (1 2 3) 2)) "Second parameter of subseq is a list")
  (verify-true (errorp (subseq "Hello" 2 #\error)) "Error in third parameter to subseq")
  (verify-true (errorp (subseq "Hello" 2 #\A)) "Third parameter of subseq is not integer")
  (verify-true (errorp (subseq "Hello" 2 (1 2 3))) "Third parameter of subseq is a list")
  (verify-true (errorp (subseq "Hello" 3 2)) "Subseq start is greater than end")
  (verify-true (errorp (subseq "Hello" -1 2)) "Subseq start is less than zero")
  (verify-true (errorp (string-upcase)) "No parameters to string-upcase")
  (verify-true (errorp (string-upcase T)) "Non-string parameter to string-upcase")
  (verify-true (errorp (string-upcase #\error)) "Error parameter to string-upcase")
  (verify-true (errorp (string-upcase (1 2 3))) "List parameter to string-upcase")
  (verify-true (errorp (string-downcase)) "No parameters to string-downcase")
  (verify-true (errorp (string-downcase T)) "Non-string parameter to string-downcase")
  (verify-true (errorp (string-downcase (1 2 3))) "List parameter to string-downcase")
  (verify-true (errorp (string-downcase #\error)) "Error parameter to string-downcase")
)
(test-str-err)
(setq test-str-err 0)
;
;(dump)
(print "===> Testing complete")
(terpri)
(summary)
(exit)

