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
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
