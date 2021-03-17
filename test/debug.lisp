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
(print "===> Testing let")
(terpri)
(defun test-let ()
  (let ((a (+ 1 2)) (b 4) (c 0) (d (1 2 3)) (test-let "A"))
    (verify-equal 3 a "A is 3")
    (verify-equal 4 b "B is 4")
    (verify-equal 0 c "C is 0")
    (verify-equal 1 (car d) "First element of D is 1")
    (verify-equal 3 (length d) "Length of D is 3")
    (setq d (4 5 6 7))
    (verify-equal 4 (length d) "Length of D is now 4")
    (verify-equal 4 (car d) "First element of D is 4")
    (setq a (+ 1 a))
    (verify-equal 4 a "A is 4")
    (setq b -10)
    (verify-equal -10 b  "B is -10")
    (setq c (+ a 2))
    (verify-equal 6 c "C is 6")
    (verify-equal "A" test-let "Locally, test-let is <A>")
    (let ((a 1))
      (verify-equal 1 a "A is 1")
      (setq a 20)
      (verify-equal 20 a "A is 20"))
    (verify-equal 4 a "A is 4")))
(test-let)
(setq test-let 0)
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
