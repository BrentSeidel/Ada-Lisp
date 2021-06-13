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
;  Test eval operation
;
(print "===> Testing eval operations")
(terpri)
(defun test-eval ()
  (verify-equal 4 (eval (read "(+ 1 2 1)")) "Simple addition")
  (verify-true (errorp (eval)) "No parameter to eval")
  (verify-equal 1 (eval 1) "Integer parameter to eval")
  (verify-true (errorp (eval (read "(+ 1 2"))) "Error passed to eval")
  (verify-equal 0 (eval *FAIL-COUNT*) "There should be no failures")
)
(test-eval)
(setq test-eval 0)
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
