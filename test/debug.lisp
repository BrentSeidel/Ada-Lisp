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
;  Test error conditions.  There are lots so they will be broken down into smaller
;  groups.
;
;
;
(print "===> Testing stack overflow")
(terpri)
(defun test-stack-ovr (a) (print "A is " a) (terpri) (test-stack-ovr (+ 1 a)))
(verify-equal ERR_STACK (test-stack-ovr 1) "Recursive function stack overflow error")
(defun test-stack-ovr (a) (let (A1 A2 A3 A4 A5 A6 A7 A8 A9 A10) (print "A is " a) (terpri) (test-stack-ovr (+ 1 a))))
(verify-equal ERR_STACK (test-stack-ovr 1) "Recursive function with locals stack overflow error")
(verify-equal ERR_WRONGTYPE (let (A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14
  A15 A16 A17 A18 A19 A20 A21 A22 A23 A24 A25 A26 A27 A28 A29 A30 A31 A32
  A33 A34 A35 A36 A37 A38 A39 A40 A41 A42 A43 A44 A45 A46 A47 A48 A49 A50
  A51 A52 A53 A54 A55 A56 A57 A58 A59 A60 A61 A62 A63 A64 A65 A66 A67 A68
  A69 A70 A71 A72 A73 A74 A75 A76 A77 A78 A79 A80 A81 A82 A83 A84 A85 A86
  A87 A88 A89 A90 A91 A92 A93 A94 A95 A96 A97 A98 A99 A100)
  (print "So many local variables") (terpri)) "Too many local variables")
(defun test-lambda (a b)
  (verify-equal a (b 1 2 3 4 5 6 7 8 9
  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33
  34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57
  58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
  82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) "Testing lambda"))
(verify-equal ERR_WRONGTYPE (test-lambda T (lambda (A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14
  A15 A16 A17 A18 A19 A20 A21 A22 A23 A24 A25 A26 A27 A28 A29 A30 A31 A32
  A33 A34 A35 A36 A37 A38 A39 A40 A41 A42 A43 A44 A45 A46 A47 A48 A49 A50
  A51 A52 A53 A54 A55 A56 A57 A58 A59 A60 A61 A62 A63 A64 A65 A66 A67 A68
  A69 A70 A71 A72 A73 A74 A75 A76 A77 A78 A79 A80 A81 A82 A83 A84 A85 A86
  A87 A88 A89 A90 A91 A92 A93 A94 A95 A96 A97 A98 A99 A100)
  (print "So many parameters") (terpri))) "Lambda variables")
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(summary)
(exit)
