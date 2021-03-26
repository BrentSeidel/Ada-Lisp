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
(print "===> Testing I/O operations")
(terpri)
(defun test-io ()
  (verify-equal NIL (print 1) "Print integer returns NIL")
  (verify-equal NIL (print #\1) "Print character returns NIL")
  (verify-equal NIL (print "1") "Print string returns NIL")
  (verify-equal NIL (print 'print) "Print quoted symbol returns NIL")
  (verify-equal NIL (print print) "Print builting symbol returns NIL")
  (verify-equal NIL (print (1 2 3)) "Print list returns NIL")
  (verify-equal NIL (print ()) "Print empty list returns NIL")
  (let (a)
    (verify-equal NIL (print a) "Print empty local variable returns NIL")
    (setq a (1 2 3))
    (verify-equal NIL (print a) "Print list local variable returns NIL")
    (setq a 'print)
    (verify-equal NIL (print a) "Print quoted symbol local variable returns NIL")
    (setq a print)
    (verify-equal NIL (print a) "Print symbol local variable returns NIL")
    (setq a (lambda (c) (+ 1 c)))
    (verify-equal NIL (print a) "Print lambda local variable returns NIL")
    (verify-equal NIL (print b) "Print unknown variable returns NIL"))
  (verify-equal NIL (terpri) "Terpri returns NIL")
  (verify-equal NIL (fresh-line) "Fresh-line returns NIL")
  (verify-equal NIL (progn (print "Hello") (fresh-line)) "Fresh-line returns NIL")
  (verify-equal "Testing a longer line that should be split across fragments."
    (read-line) "Text read from read-line"))
(test-io)
Testing a longer line that should be split across fragments.
(setq test-io 0)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
