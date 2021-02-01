;
;  Test functions for the Tiny Lisp Interpreter.  Currently, the data table
;  probably aren't large enough to load all this code at once.  To run a test
;  set, first start the Lisp interpreter, then load the support functions.
;  Finally load and run the desired test.
;
;  These tests more or less proceed in increasing complexity where later tests
;  depend on items from earlier tests working correctly.  There are some things
;  that are not explicitly tested, such as defun, but are used throughout so
;  failures should be obvious with other things not working properly.
;
;-----------------------------------------
;  Support functions.  Load these first.
;
; Prints a pass message
(defun pass (n)
  (print "PASS: " n)
  (terpri))
; Prints a fail message
(defun fail (n)
  (print "***FAIL: " n)
  (terpri))
;
;  If two values are equal, print pass message, otherwise print fail message
;
(defun verify-equal (expected actual text)
  (if (= actual expected) (print "PASS: Actual ")
              (print "***FAIL: Actual "))
  (print  actual ", Expected " expected " " text)
  (terpri))
;  --------------------------------------------
;  Test cases.  Load and run the desired test.
;
;  Test if and booleans
;
(defun test-bool ()
  (if t (pass "T literal works")
        (fail "T literal does not work"))
  (if nil (fail "NIL literal does not work")
          (pass "NIL literal works")))
;
(print "===> Testing booleans")
(terpri)
(test-bool)
(setq test-bool 0)
;
;  Test integer comparisons
;
(defun test-int-cmp ()
  (if (= 0 0) (pass "0 = 0") (fail "0 = 0"))
  (if (< 0 0) (fail "0 < 0") (pass "0 < 0"))
  (if (> 0 0) (fail "0 > 0") (pass "0 > 0"))
  (if (/= 0 0) (fail "0 /= 0") (pass "0 /= 0"))
  (if (= 2 1) (fail "2 = 1") (pass "2 = 1"))
  (if (< 2 1) (fail "2 < 1") (pass "2 < 1"))
  (if (> 2 1) (pass "2 > 1") (fail "2 > 1"))
  (if (/= 2 1) (pass "2 /= 1") (fail "2 /= 1"))
  (if (= -1 1) (fail "-1 = 1") (pass "-1 = 1"))
  (if (< -1 1) (pass "-1 < 1") (fail "-1 < 1"))
  (if (> -1 1) (fail "-1 > 1") (pass "-1 > 1"))
  (if (/= -1 1) (pass "-1 /= 1") (fail "-1 /= 1"))
  (if (= -2 -1) (fail "-2 = -1") (pass "-2 = -1"))
  (if (< -2 -1) (pass "-2 < -1") (fail "-2 < -1"))
  (if (> -2 -1) (fail "-2 > -1") (pass "-2 > -1"))
  (if (/= -2 -1) (pass "-2 /= -1") (fail "-2 /= -1")))
;
(print "===> Testing integer comparisons")
(terpri)
(test-int-cmp)
(setq test-int-cmp 0)
;
;  Test string comparisons
;
(defun test-str-cmp ()
  (if (= "A" "A") (pass "A = A") (fail "A = A"))
  (if (< "A" "A") (fail "A < A") (pass "A < A"))
  (if (> "A" "A") (fail "A > A") (pass "A > A"))
  (if (/= "A" "A") (fail "A /= A") (pass "A /= A"))
  (if (= "A" "B") (fail "A = B") (pass "A = B"))
  (if (< "A" "B") (pass "A < B") (fail "A < B"))
  (if (> "A" "B") (fail "A > B") (pass "A > B"))
  (if (/= "A" "B") (pass "A /= B") (fail "A /= B"))
  (if (= "A" "AA") (fail "A = AA") (pass "A = AA"))
  (if (< "A" "AA") (pass "A < AA") (fail "A < AA"))
  (if (> "A" "AA") (fail "A > AA") (pass "A > AA"))
  (if (/= "A" "AA") (pass "A /= AA") (fail "A /= AA")))
;
(print "===> Testing string comparisons")
(terpri)
(test-str-cmp)
(setq test-str-cmp 0)
;
;  Test basic math functions
;
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
(print "===> Testing basic math operations")
(terpri)
(test-basic-math)
(setq test-basic-math 0)
;
;  Test global variables with setq
;
(defun test-global ()
  (setq **GLOB** 1)
  (verify-equal 1 **GLOB** "Set global variable")
  (setq **GLOB** (+ 1 **GLOB**))
  (verify-equal 2 **GLOB** "Increment global")
  (setq **GLOB** "Hello")
  (verify-equal "Hello" **GLOB** "Set global to string"))
;
(print "===> Testing globals")
(terpri)
(test-global)
(setq test-global 0)
;
;  Test local variables with setq
;
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
;
(print "===> Testing let")
(terpri)
(test-let)
(setq test-let 0)
;
;  Test dowhile operation
;
(defun test-dowhile ()
  (let ((count 0) (accum 0))
    (dowhile (< count 10)
      (setq count (+ count 1))
      (setq accum (+ accum count))
      (print "Counter is " count ", Accumulator is " accum)
      (terpri))
    (verify-equal 10 count "Count is 10")
    (verify-equal 55 accum "Accumulator is 55")))
;
(print "===> Testing dowhile")
(terpri)
(test-dowhile)
(setq test-dowhile 0)
;
;  Test dotimes operation
;
(defun test-dotimes ()
  (let ((accum 0) result)
    (setq result (dotimes (n 10 20)
      (print "Count is " n)
      (terpri)
      (setq accum (+ accum n))))
    (verify-equal 20 result "Result returned")
    (verify-equal 45 accum "Accumulator is 45")))
;
(print "===> Testing dotimes")
(terpri)
(test-dotimes)
(setq test-dotimes 0)
;
;  Test logical operations
;
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
;
(print "===> Testing logic operations")
(terpri)
(test-logic)
(setq test-logic 0)
;
;  Test character data type and operations
;
(defun test-char ()
  (if (= #\A #\A) (pass "A = A") (fail "A /= A"))
  (if (= #\A #\B) (fail "A = B") (pass "A /= B"))
  (if (< #\A #\B) (pass "A < B") (fail "A /< B"))
  (if (< #\A #\A) (fail "A < A") (pass "A /< A"))
  (if (> #\B #\A) (pass "B > A") (fail "B /> A"))
  (if (> #\B #\B) (fail "B > B") (pass "B /> B"))
  (if (/= #\A #\N) (pass "A /= N") (fail "A = N"))
  (if (/= #\A #\A) (fail "A /= A") (pass "A = A"))
  (verify-equal (char-code #\A) 65 "A has character code 65")
  (verify-equal (code-char 66) #\B "Character code 66 is B")
  (verify-equal (char-downcase #\A) #\a "Lower case A is a")
  (verify-equal (char-upcase #\a) #\A "Upper case a is A"))
;
(print "===> Testing characters")
(terpri)
(test-char)
(setq test-char 0)
;
;  Test string operations
;
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
  (verify-equal "HELLO" (string-upcase "hello") "Upper case text")
  (verify-equal "hello" (string-downcase "HELLO") "Lower case text")
  (verify-equal "-bye" (subseq "Good-bye" 4) "Subsequence with default end")
  (verify-equal "d-b" (subseq "Good-bye" 3 6) "Subsequence with specified end"))
;
(print "===> Testing strings")
(terpri)
(test-str)
(setq test-str 0)
;
;  Test predicates
;
(defun test-pred ()
  (if (arrayp (1 3 4)) (fail "arrayp") (pass "arrayp"))
  (if (bit-vector-p (1 2 3)) (fail "bit-vector-p") (pass "bit-vector-p"))
  (if (complexp 1) (fail "complexp") (pass "complexp"))
  (if (floatp (/ 1 3)) (fail "floatp") (pass "floatp"))
  (if (vectorp (1 2 3)) (fail "vectorp") (pass "vectorp"))
  (if (rationalp (/ 2 5)) (fail "rationalp") (pass "rationalp"))
  (if (realp (/ 5 2)) (fail "realp") (pass "realp"))
  (if (simple-vector-p (1 2 3)) (fail "simple-vector-p") (pass "simple-vector-p"))
  (if (simple-bit-vector-p #x0F0F0F0F) (fail "simple-bit-vector-p") (pass "simple-bit-vector-p"))
  (if (packagep "main") (fail "packagep") (pass "packagep"))
  (if (vectorp (1 2 3)) (fail "vectorp") (pass "vectorp"))
  (if (atomp 1) (pass "atomp is true") (fail "atomp is false"))
  (if (atomp (1)) (fail "atomp is true") (pass "atomp is false"))
  (if (characterp #\A) (pass "characterp is true") (fail "characterp is false"))
  (if (characterp 1) (fail "characterp is true") (pass "characterp is false"))
  (if (compiled-function-p print) (pass "compiled-function-p is true") (fail "compiled-function-p is false"))
  (if (compiled-function-p test-pred) (fail "compiled-function-p is true") (pass "compiled-function-p is false"))
  (if (consp (1 2 3)) (pass "consp is true") (fail "consp is false"))
  (if (consp 1) (fail "consp is true") (pass "consp is false"))
  (if (functionp test-pred) (pass "functionp is true") (fail "functionp is false"))
  (if (functionp 1) (fail "functionp is true") (pass "functionp is false"))
  (if (integerp 3) (pass "integerp is true") (fail "integerp is false"))
  (if (integerp #\A) (fail "integerp is true") (pass "integerp is false"))
  (if (listp (2 4 6)) (pass "listp is true") (fail "listp is false"))
  (if (listp "Hello") (fail "listp is true") (pass "listp is false"))
  (if (numberp 10) (pass "numberp is true") (fail "numberp is false"))
  (if (numberp #\B") (fail numberp is true") (pass "numberp is false"))
  (if (null ()) (pass "null is true") (fail "null is false"))
  (if (null 1) (fail "null is true") (pass "null is false"))
  (if (simple-string-p "Hello") (pass "simple-string-p is true") (fail "simple-string-p is false"))
  (if (simple-string-p 2) (fail "simple-string-p is true") (pass "simple-string-p is false"))
  (if (stringp "Hello") (pass "simple-string-p is true") (fail "simple-string-p is false"))
  (if (stringp #\C) (fail "simple-string-p is true") (pass "simple-string-p is false"))
  (if (symbolp car) (pass "symbolp is true") (fail "symbolp is false"))
  (if (symbolp #\@) (fail "symbolp is true") (pass "symbolp is true")))
;
(print "===> Testing predicates")
(terpri)
(test-pred)
(setq test-pred 0)
;
;  Test coerce and quoted symbols
;
(defun test-coerce ()
  (verify-equal 1 (coerce t 'integer) "Boolean to integer")
  (verify-equal 0 (coerce nil 'integer) "Boolean to integer")
  (verify-equal "T" (coerce t 'string) "Boolean to string")
  (verify-equal "NIL" (coerce nil 'string) "Boolean to string")
  (verify-equal "A" (coerce #\A 'string) "Character to string")
  (verify-equal "@" (coerce #\@ 'string) "Character to string")
  (verify-equal T (coerce 1 'boolean) "Integer to boolean")
  (verify-equal NIL (coerce 0 'boolean) "Integer to boolean"))
;
(print "===> Testing coercion")
(terpri)
(test-coerce)
(setq test-coerce 0)
;
;  Test concatenation of strings.  Lists can't be compared so
;  they aren't tested.
;
(defun test-concatenate ()
  (verify-equal "Hello world!" (concatenate 'string "Hello" " " "world" "!") "String concatenation"))
;
(print "==> Testing concatenation")
(terpri)
(test-concatenate)
(setq test-concatenate 0)
;
;(dump)
(exit)

