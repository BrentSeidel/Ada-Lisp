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
;  Test list operations.  Since list comparisons aren't currently supported,
;  these aren't quite as nice as they might be.
;
(print "===> Testing list operations")
(terpri)
(defun test-list ()
  (verify-equal 3 (length (1 2 3)) "Length of a raw list")
  (verify-equal 4 (length (quote "a" "B" 3 4)) "Length of a quoted list")
  (verify-equal 5 (car (5 1 2 3 4)) "CAR of a list")
  (verify-equal 4 (length (cdr (5 6 7 8 9))) "Length of CDR")
  (verify-equal 2 (length (cons (2 3 4) 1)) "CONS of elements")
  (verify-equal 4 (length (cons 1 (2 3 4))) "CONS of elements")
  (verify-equal 1 (car (cons 1 (2 3 4))) "CAR of a CONS")
  (verify-equal 2 (car (cdr (1 2 3 4))) "CAR of a CDR of a list")
  (verify-equal 5 (car 5) "CAR of a single value")
  (verify-equal NIL (car) "CAR of nothing is nothing")
  (verify-equal NIL (cdr) "CDR of nothing is nothing")
  (varify-equal NIL (cdr 1) "CDR of a value is nothing")
  (verify-equal ERR_NOPARAM (cons) "No parameter to CONS")
  (verify-equal ERR_PARSECHAR (cons #\error 1) "CONS error in first parameter")
  (verify-equal ERR_PARSECHAR (cons 1 #\error) "CONS error in second parameter")
  (verify-equal ERR_PARSECHAR (list #\error 1 2 3) "LIST error in first parameter")
  (verify-equal ERR_PARSECHAR (list 1 2 #\error 3) "LIST error in later parameter")
  (verify-equal NIL (list) "LIST with no parameters is nothing")
  (verify-equal 5 (car (rplaca (1 2 3 4) 5)) "Replace CAR of first element in list")
  (verify-equal 2 (car (cdr (rplaca (1 2 3 4) 5))) "Replace CAR of first element in list (rest of list)")
  (verify-equal 6 (car (cdr (rplacd (1 2 3 4) (6 7)))) "Replace CDR of first element in list (rest of list)")
  (verify-equal ERR_NOPARAM (rplaca) "RPLACA with no parameters")
  (verify-equal ERR_WRONGTYPE (rplaca 1 1) "RPLACA needs a list")
  (verify-equal ERR_NOPARAM (rplacd) "RPLACD with no parameters")
  (verify-equal ERR_WRONGTYPE (rplacd 1 1) "RPLACD needs a list"))
(test-list)
(setq test-list 0)
;
;--------------------------------------------
(print "===> Testing complete")
(terpri)
(summary)
(exit)
