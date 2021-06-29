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
(print "==> Testing string operation error conditions")
(terpri)
(defun test-log-err ()
  (verify-true (errorp (char)) "No parameter to char")
  (verify-true (errorp (char print 1)) "Symbol first parameter type to char")
  (verify-true (errorp (char "A" print)) "Symbol second parameter type to char")
  (verify-true (errorp (char 1 1)) "Invalid first parameter type to char")
  (verify-true (errorp (char "A" "A")) "Invalid second parameter type to char")
  (verify-true (errorp (char "AB" 10)) "Out of range second parameter type to char")
  (verify-true (errorp (parse-integer)) "No parameter to parse-integer")
  (verify-true (errorp (parse-integer 1)) "Non string parameter to parse-integer")
  (verify-true (errorp (parse-integer #\error)) "Error string parameter to parse-integer")
  (verify-true (errorp (string-downcase)) "No parameter to string-downcase")
  (verify-true (errorp (string-downcase 1)) "Invalid parameter type to string-downcase")
  (verify-true (errorp (string-upcase)) "No parameter to string-upcase")
  (verify-true (errorp (string-upcase \#b)) "Invalid parameter type to string-upcase")
  (verify-true (errorp (subseq)) "No parameter type to subseq")
  (verify-true (errorp (subseq 1 2 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "A" 2 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "ABC" #\a 3)) "Invalid parameter type to subseq")
  (verify-true (errorp (subseq "ABC" 1 #\b)) "Invalid parameter type to subseq"))
(test-log-err)
(setq test-log-err 0)
;(dump)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(exit)
