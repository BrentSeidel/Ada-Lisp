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
;(defun verify-false (act text-msg)
;  (verify-equal NIL act text-msg))
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
;
;  Test lambda and function passing
;
(print "==> Testing Lambda and function passing")
(terpri)
(defun test-lambda (a b c d)
  (print "Expected value is " a)
  (terpri)
  (print "Actual value is " (b c d))
  (terpri)
  (verify-equal a (b c d) "Testing lambda"))
(defun add (a b) (+ a b))
(test-lambda 3 (lambda (a1 a2) (+ a1 a2)) 1 2)
(test-lambda 9 add 4 5)
(test-lambda 6 (lambda (a1 a2) (* a1 a2)) 2 3)
(test-lambda 12 * 3 4)
(test-lambda T (lambda (a1 a2) (< a1 a2)) #\A #\B)
(test-lambda T (lambda (a1 a2)(= a1 (* a2 (/ a1 a2)))) 6 2)
(setq test-lambda 0)
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
