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
;  Test read expression operation
;
(print "===> Testing read expression operations")
(terpri)
(defun test-read ()
  (let ((temp (read "(1 2 3)")))
    (verify-equal 3 (length temp) "Correct number of items parsed")
    (verify-equal 1 (car temp) "First item is correct"))
  (let ((temp (read "(+ 1 2 3 4 -5 -6 -7 -8)")))
    (verify-equal 9 (length temp) "Longer list parsed")
    (verify-equal + (car temp) "First item is addition symbol"))
  (verify-true (errorp (read)) "Read needs a parameter")
  (verify-true (errorp (read 1)) "Read parameter needs to be a string")
  (verify-true (errorp (read "(1 2 3")) "Parse list error")
  (verify-true (errorp (read "(1 2 ; Comment)")) "Parse with a comment")
  (verify-equal 123 (read "123") "Read an integer")
  (verify-true (read "t") "Read true")
  (verify-false (read "nil") "Read nil")
)
(test-read)
(setq test-read 0)
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
