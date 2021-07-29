;
;  This is a subset of test.lisp to make it easier to debug specific
;  test cases without wading through all the other results.
;
;  ./lisp < debug.lisp
;
;  This file is expected to change frequently as different tests are
;  being developed or debugged.
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
  (if (= expected actual)
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
;(defun summary ()
;  (print "Test cases passed: " *PASS-COUNT*)
;  (terpri)
;  (print "Test cases failed: " *FAIL-COUNT*)
;  (terpri)
;  (print "Total test cases:  " (+ *PASS-COUNT* *FAIL-COUNT*)))
;--------------------------------------------
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
;(dump)
(setq test-pred 0)
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
