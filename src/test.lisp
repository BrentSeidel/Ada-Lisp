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
(defun verify-equal (actual expected text)
  (if (= actual expected) (print "Pass: Actual ")
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
;  Test basic math functions
;
(defun test-basic-math ()
  (verify-equal (+ 1 2 3) 6 "Add positive")
  (verify-equal (+ 1 2 -3) 0 "Add mixed")
  (verify-equal (+ -1 -2 -3) -6 "Add negative")
  (verify-equal (* 2 3 4) 24 "Mul positive")
  (verify-equal (* -2 3 4) -24 "Mul mixed")
  (verify-equal (* -6 -4) 24 "Mul negative")
  (verify-equal (- 1 2 3) -4 "Sub result negative")
  (verify-equal (- 4 1) 3 "Sub positive")
  (verify-equal (- -2 -3) 1 "Sub negative")
  (verify-equal (- -2 3) -5 "Sub mixed")
  (verify-equal (/ 24 3 4) 2 "Div positive")
  (verify-equal (/ -24 3) -8 "Div mixed")
  (verify-equal (/ -24 -3) 8 "Div negative")
  (verify-equal (/ 24 5) 4 "Div inexact"))
;
;  Test global variables with setq
;
(defun test-global ()
  (setq **GLOB** 1)
  (verify-equal **GLOB** 1 "Set global variable")
  (setq **GLOB** (+ 1 **GLOB**))
  (verify-equal **GLOB** 2 "Increment global")
  (setq **GLOB** "Hello")
  (verify-equal **GLOB** "Hello" "Set global to string"))
;
;  Test local variables with setq
;
(defun test-let ()
  (let ((a (+ 1 2)) (b 4) (c 0))
    (verify-equal a 3 "A is 3")
    (verify-equal b 4 "B is 4")
    (verify-equal c 0 "C is 0")
    (setq a (+ 1 a))
    (verify-equal a 4 "A is 4")
    (setq b -10)
    (verify-equal b -10 "B is -10")
    (setq c (+ a 2))
    (verify-equal c 6 "C is 6")
    (let ((a 1))
      (verify-equal a 1 "A is 1")
      (setq a 20)
      (verify-equal a 20 "A is 20"))
    (verify-equal a 4 "A is 4")))
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
    (verify-equal count 10 "Count is 10")
    (verify-equal accum 55 "Accumulator is 55")))
;
;  Test dotimes operation
;
(defun test-dotimes ()
  (let ((accum 0) result)
    (setq result (dotimes (n 10 20)
      (print "Count is " n)
      (terpri)
      (setq accum (+ accum n))))
    (verify-equal result 20 "Result returned")
    (verify-equal accum 45 "Accumulator is 45")))
;
;  Test logical operations
;
(defun test-logic ()
  (verify-equal (not T) NIL "T -> NIL")
  (verify-equal (not NIL) T "NIL -> T")
  (verify-equal (not #xFFFF0000) #x0000FFFF "#xFFFF0000 -> #x0000FFFF")
  (verify-equal (not #x5A5A5A5A) #xA5A5A5A5 "#x5A5A5A5A -> #xA5A5A5A5")
  (verify-equal (and NIL NIL) NIL "NIL and NIL -> NIL")
  (verify-equal (and NIL T) NIL "NIL and T -> NIL")
  (verify-equal (and T NIL) NIL "T and NIL -> NIL")
  (verify-equal (and T T) T "T and T -> T")
  (verify-equal (or NIL NIL) NIL "NIL or NIL -> NIL")
  (verify-equal (or NIL T) T "NIL or T -> T")
  (verify-equal (or T NIL) T "T or NIL -> T")
  (verify-equal (or T T) T "T or T -> T")
  (verify-equal (and 15 4 3) 0 "Produces zero")
  (verify-equal (and #xF0F #xF01F) 15 "Produces 15")
  (verify-equal (or 1 2 4) 7 "Produces 7")
  (verify-equal (or #xF000 #xF0) #xF0F0 "Produces #xF0F0")
  (verify-equal (not (and T NIL)) T "T nand NIL -> T"))
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
  (verify-equal (char-int #\A) 65 "A has character code 65")
  (verify-equal (int-char 66) #\B "Character code 66 is B")
  (verify-equal (char-downcase #\A) #\a "Lower case A is a")
  (verify-equal (char-upcase #\a) #\A "Upper case a is A"))
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
  (if (nullp ()) (pass "nullp is true") (fail "nullp is false"))
  (if (nullp 1) (fail "nullp is true") (pass "nullp is false"))
  (if (simple-string-p "Hello") (pass "simple-string-p is true") (fail "simple-string-p is false"))
  (if (simple-string-p 2) (fail "simple-string-p is true") (pass "simple-string-p is false"))
  (if (stringp "Hello") (pass "simple-string-p is true") (fail "simple-string-p is false"))
  (if (stringp #\C) (fail "simple-string-p is true") (pass "simple-string-p is false"))
  (if (symbolp car) (pass "symbolp is true") (fail "symbolp is false"))
  (if (symbolp #\@) (fail "symbolp is true") (pass "symbolp is true")))

