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
(defun summary ()
  (print "Test cases passed: " *PASS-COUNT*)
  (terpri)
  (print "Test cases failed: " *FAIL-COUNT*)
  (terpri)
  (print "Total test cases:  " (+ *PASS-COUNT* *FAIL-COUNT*)))
;--------------------------------------------
;
;  Test the condition operation.
;
(print "===> Testing cond operations")
(terpri)
(defun test-cond ()
  (verify-equal ERR_FEWPARAM
    (cond
      ((= 1 2) (+ 1 2))
      ((= 2 3) (+ 2 3))
      ((= 3 4) (+ 3 4)))
    "No condition branch matches")
  (verify-equal ERR_WRONGTYPE
    (cond
      ((= 1 2) (+ 1 2))
      5
      ((= 3 4) (+ 3 4)))
    "Non-list element encountered")
  (verify-equal ERR_HARDWARE
    (cond
      ((= 1 2) (+ 1 2))
      ERR_HARDWARE
      ((= 3 4) (+ 3 4)))
    "Error evaluating candidate encountered")
  (verify-equal ERR_HARDWARE
    (cond
      ((= 1 2) (+ 1 2))
      (ERR_HARDWARE (+ 2 3))
      ((= 3 4) (+ 3 4)))
    "Error evaluating condition encountered")
  (verify-equal 5
    (cond
      ((= 1 2) (+ 1 2))
      ((= 3 3) (+ 1 2) (+ 2 3))
      ((= 3 4) (+ 3 4)))
    "One item matches")
  (verify-equal 5
    (cond
      ((= 1 2) (+ 1 2))
      ((= 3 3) (+ 2 3))
      ((= 4 4) (+ 3 4)))
    "Two items match, only first is evaluated")
)
(test-cond)
(setq test-cond 0)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(summary)
(exit)
