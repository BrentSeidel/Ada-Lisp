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
(setq *PASS-COUNT* 0)
(setq *FAIL-COUNT* 0)
;
; Prints a pass message
(defun pass (n)
  (setq *PASS-COUNT* (+ *PASS-COUNT* 1))
  (print "PASS: " n)
  (terpri))
; Prints a fail message
(defun fail (n)
  (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1))
  (print "***FAIL: " n)
  (terpri))
;
;  If two values are equal, print pass message, otherwise print fail message
;
(defun verify-equal (expected actual text)
  (if (= actual expected) (setq *PASS-COUNT* (+ *PASS-COUNT* 1))
              (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)))
  (if (= actual expected) (print "PASS: Actual ")
              (print "***FAIL: Actual "))
  (print  actual ", Expected " expected " " text)
  (terpri))
;
;  Print summary results
;
(defun summary ()
  (print "Test cases passed: " *PASS-COUNT*)
  (terpri)
  (print "Test cases failed: " *FAIL-COUNT*)
  (terpri)
  (print "Total test cases:  " (+ *PASS-COUNT* *FAIL-COUNT*)))
;  --------------------------------------------
;  Test cases.  Load and run the desired test.
;
;  Test if and booleans
;
(print "===> Testing booleans")
(terpri)
(defun test-bool ()
  (if t (pass "T literal works")
        (fail "T literal does not work"))
  (if nil (fail "NIL literal does not work")
          (pass "NIL literal works")))
(test-bool)
(setq test-bool 0)
;
;  Test integer comparisons
;
(print "===> Testing integer comparisons")
(terpri)
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
(test-int-cmp)
(setq test-int-cmp 0)
;
;  Test string comparisons
;
(print "===> Testing string comparisons")
(terpri)
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
(test-str-cmp)
(setq test-str-cmp 0)
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
;  Test character data type and operations
;
(print "===> Testing characters")
(terpri)
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
  (verify-equal "HELLO" (string-upcase "hello") "Upper case text")
  (verify-equal "hello" (string-downcase "HELLO") "Lower case text")
  (verify-equal "-bye" (subseq "Good-bye" 4) "Subsequence with default end")
  (verify-equal "d-b" (subseq "Good-bye" 3 6) "Subsequence with specified end"))
(test-str)
(setq test-str 0)
;
;  Test predicates
;
(print "===> Testing predicates")
(terpri)
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
  (if (errorp 1) (fail "errorp is true") (pass "errorp is false"))
  (if (errorp (+ 1 "a")) (pass "errorp is true") (fail "errorp is false"))
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
  (verify-equal "T" (coerce t 'string) "Boolean to string")
  (verify-equal "NIL" (coerce nil 'string) "Boolean to string")
  (verify-equal "A" (coerce #\A 'string) "Character to string")
  (verify-equal "@" (coerce #\@ 'string) "Character to string")
  (verify-equal T (coerce 1 'boolean) "Integer to boolean")
  (verify-equal NIL (coerce 0 'boolean) "Integer to boolean"))
(test-coerce)
(setq test-coerce 0)
;
;  Test concatenation of strings.  Lists can't be compared so
;  they aren't tested.
;
(print "==> Testing concatenation")
(terpri)
(defun test-concatenate ()
  (verify-equal "Hello world!" (concatenate 'string "Hello" " " "world" "!") "String concatenation"))
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

(defun check-eq (a b) (if (= a b) (print "Equal") (print "Not-equal")) (terpri))
;
;  Test error conditions.  There are lots so they will be broken down into smaller
;  groups.
;
(print "==> Testing comparison error conditions")
(terpri)
(defun test-cmp-err ()
  (verify-equal T (errorp (=)) "No parameters to equal")
  (verify-equal T (errorp (= 1)) "One parameters to equal")
  (verify-equal T (errorp (= 1 "A")) "Mismatched parameters to equal")
  (verify-equal T (errorp (/=)) "No parameters to not-equal")
  (verify-equal T (errorp (/= 1)) "One parameters to not-equal")
  (verify-equal T (errorp (/= 1 "A")) "Mismatched parameters to not-equal")
  (verify-equal T (errorp (<)) "No parameters to less-than")
  (verify-equal T (errorp (< 1)) "One parameters to less-than")
  (verify-equal T (errorp (< 1 "A")) "Mismatched parameters to less-than")
  (verify-equal T (errorp (>)) "No parameters to greater-than")
  (verify-equal T (errorp (> 1)) "One parameters to greater-than")
  (verify-equal T (errorp (> 1 "A")) "Mismatched parameters to greater-than"))
(test-cmp-err)
(setq test-cmp-err 0)
;
(print "==> Testing math error conditions")
(terpri)
(defun test-math-err ()
  (verify-equal T (errorp (+)) "No parameters to addition")
  (verify-equal T (errorp (+ 1 "A")) "Mismatched parameters to addition")
  (verify-equal T (errorp (-)) "No parameters to subtraction")
  (verify-equal T (errorp (- 1 "A")) "Mismatched parameters to subtraction")
  (verify-equal T (errorp (/)) "No parameters to division")
  (verify-equal T (errorp (/ 1 "A")) "Mismatched parameters to division")
  (verify-equal T (errorp (*)) "No parameters to multiplication")
  (verify-equal T (errorp (* 1 "A")) "Mismatched parameters to multiplication"))
(test-math-err)
(setq test-math-err 0)
;
(print "==> Testing logical operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-equal T (errorp (not)) "No parameter to NOT")
  (verify-equal T (errorp (not "A")) "Invalid parameter type to NOT")
  (verify-equal T (errorp (and)) "No parameter to AND")
  (verify-equal T (errorp (and "A")) "Invalid parameter type to AND")
  (verify-equal T (errorp (and T 1)) "Mixed parameter types to AND")
  (verify-equal T (errorp (or)) "No parameter to OR")
  (verify-equal T (errorp (or "A")) "Invalid parameter type to OR")
  (verify-equal T (errorp (or 1 NIL)) "Mixed parameter types to OR"))
(test-log-err)
(setq test-log-err 0)
;
(print "==> Testing character operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-equal T (errorp (char-code)) "No parameter to char-code")
  (verify-equal T (errorp (char-code "A")) "Invalid parameter type to char-code")
  (verify-equal T (errorp (code-char)) "No parameter to code-char")
  (verify-equal T (errorp (code-char "A")) "Invalid parameter type to code-char")
  (verify-equal T (errorp (char-downcase)) "No parameter to char-downcase")
  (verify-equal T (errorp (char-downcase "A")) "Invalid parameter type to char-downcase")
  (verify-equal T (errorp (char-upcase)) "No parameter to char-upcase")
  (verify-equal T (errorp (char-upcase "A")) "Invalid parameter type to char-upcase"))
(test-log-err)
(setq test-log-err 0)
;
(print "==> Testing string operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-equal T (errorp (char)) "No parameter to char")
  (verify-equal T (errorp (char "A" \#A)) "Invalid parameter type to char")
  (verify-equal T (errorp (char 1 "A")) "Invalid parameter type to char")
  (verify-equal T (errorp (parse-integer)) "No parameter to parse-integer")
  (verify-equal T (errorp (string-downcase)) "No parameter to string-downcase")
  (verify-equal T (errorp (string-downcase 1)) "Invalid parameter type to string-downcase")
  (verify-equal T (errorp (string-upcase)) "No parameter to string-upcase")
  (verify-equal T (errorp (string-upcase \#b)) "Invalid parameter type to string-upcase")
  (verify-equal T (errorp (subseq)) "No parameter type to subseq")
  (verify-equal T (errorp (subseq 1 2 3)) "Invalid parameter type to subseq")
  (verify-equal T (errorp (subseq "A" 2 3)) "Invalid parameter type to subseq")
  (verify-equal T (errorp (subseq "ABC" #\a 3)) "Invalid parameter type to subseq")
  (verify-equal T (errorp (subseq "ABC" 1 #\b)) "Invalid parameter type to subseq"))
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
;  (verify-equal T (errorp (arrayp)) "No parameter to arrayp")
;  (verify-equal T (errorp (bit-vector-p)) "No parameter to bit-vector-p")
;  (verify-equal T (errorp (complexp)) "No parameter to complexp")
;  (verify-equal T (errorp (floatp)) "No parameter to floatp")
;  (verify-equal T (errorp (vectorp)) "No parameter to vectorp")
;  (verify-equal T (errorp (rationalp)) "No parameter to rationalp")
;  (verify-equal T (errorp (realp)) "No parameter to realp")
;  (verify-equal T (errorp (simple-vector-p)) "No parameter to simple-vector-p")
;  (verify-equal T (errorp (simple-bit-vector-p)) "No parameter to simple-bit-vector-p")
;  (verify-equal T (errorp (packagep)) "No parameter to packagep")
;  (verify-equal T (errorp (vectorp)) "No parameter to vectorp")
  (verify-equal T (errorp (atomp)) "No parameter to atomp")
  (verify-equal T (errorp (characterp)) "No parameter to characterp")
  (verify-equal T (errorp (compiled-function-p)) "No parameter to compiled-function-p")
  (verify-equal T (errorp (consp)) "No parameter to consp")
  (verify-equal T (errorp (errorp)) "No parameter to errorp")
  (verify-equal T (errorp (functionp)) "No parameter to functionp")
  (verify-equal T (errorp (integerp)) "No parameter to integerp")
  (verify-equal T (errorp (listp)) "No parameter to listp")
  (verify-equal T (errorp (numberp)) "No parameter to numberp")
  (verify-equal T (errorp (null)) "No parameter to null")
  (verify-equal T (errorp (simple-string-p)) "No parameter to simple-string-p")
  (verify-equal T (errorp (stringp)) "No parameter to stringp")
  (verify-equal T (errorp (symbolp)) "No parameter to symbolp"))
(test-pred-err)
(setq test-pred-err 0)
;
(print "===> Testing coercion errors")
(terpri)
(defun test-coerce-err ()
  (verify-equal T (errorp (coerce)) "No paramaters")
  (verify-equal T (errorp (coerce nil)) "One parameter")
  (verify-equal T (errorp (coerce t 'print)) "Wrong coercion")
  (verify-equal T (errorp (coerce nil 'character)) "Invalid coercion"))
(test-coerce-err)
(setq test-coerce-err 0)
;
(print "==> Testing concatenation errors")
(terpri)
(defun test-concatenate-err ()
  (verify-equal T (errorp (concatenate)) "No parameters")
  (verify-equal T (errorp (concatenate 'string)) "One parameter")
  (verify-equal T (errorp (concatenate 'print "Hello " "World")) "Invalid type")
  (verify-equal T (errorp (concatenate 'list (1 2 3) "world")) "Wrong type")
  (verify-equal NIL (errorp (concatenate 'list (1 2 3) (4 5 6))) "Concatenate lists")
  (verify-equal T (errorp (concatenate 'string (1 2 3) "world")) "Wrong type")
  (verify-equal T (errorp (concatenate 'string "Hello " 1)) "Wrong type"))
(test-concatenate-err)
(setq test-concatenate-err 0)
;
;(dump)
(print "===> Testing complete")
(terpri)
(summary)
(exit)

