;
;  Test functions for the Tiny Lisp Interpreter.  Currently, the data table
;  probably aren't large enough to load all this code at once.  To run a test
;  set, first start the Lisp interpreter, then load the support functions.
;  Finally load and run the desired test.
;
;  These tests more or less proceed in increasing complexity where later tests
;  depend on items from earlier tests working correctly.  There are some things
;  that are not explicitly tested, such as defun, but are used throughout so
;  failures should be obvious with other things not working properly.
;
;
;  Support functions.  Load these first.
;
; Prints a pass message
(defun pass (n)
  (print "PASS: " n)
  (terpri))
; Prints a fail message
(defun fail (n)
  (print "***FAIL: " n)
  (terpri))
;  If two values are equal, print pass message, otherwise print fail message
(defun verify-equal (actual expected msg)
  (if (= actual expected) (print "Pass: Actual")
              (print "***FAIL: Actual"))
  (print  actual ", Expected " expected " " msg)
  (terpri))
;
;  Test cases.  Load and run the desired test.
;
;  Test if and booleans
;
(defun test-bool ()
  (if t (pass "T literal works")
        (fail "T literal does not work"))
  (if nil (fail "NIL literal does not work")
          (pass "NIL literal works")))
;
;  Test integer comparisons
;
(defun test-int-cmp ()
  (if (= 0 0) (pass "0 = 0") (fail "0 = 0"))
  (if (< 0 0) (fail "0 < 0") (pass "0 < 0"))
  (if (> 0 0) (fail "0 > 0") (pass "0 > 0"))
  (if (/= 0 0) (fail "0 /= 0") (pass "0 /= 0"))
  (if (= 2 1) (fail "2 = 1") (pass "2 = 1"))
  (if (< 2 1) (fail "2 < 1") (pass "2 < 1"))
  (if (> 2 1) (pass "2 > 1") (fail "2 > 1"))
  (if (/= 2 1) (pass "2 /= 1") (fail "2 /= 1"))
  (if (= -1 1) (fail "-1 = 1") (pass "-1 = 1"))
  (if (< -1 1) (pass "-1 < 1") (fail "-1 < 1"))
  (if (> -1 1) (fail "-1 > 1") (pass "-1 > 1"))
  (if (/= -1 1) (pass "-1 /= 1") (fail "-1 /= 1"))
  (if (= -2 -1) (fail "-2 = -1") (pass "-2 = -1"))
  (if (< -2 -1) (pass "-2 < -1") (fail "-2 < -1"))
  (if (> -2 -1) (fail "-2 > -1") (pass "-2 > -1"))
  (if (/= -2 -1) (pass "-2 /= -1") (fail "-2 /= -1")))
;
;  Test string comparisons
;
(defun test-str-cmp ()
  (if (= "A" "A") (pass "A = A") (fail "A = A"))
  (if (< "A" "A") (fail "A < A") (pass "A < A"))
  (if (> "A" "A") (fail "A > A") (pass "A > A"))
  (if (/= "A" "A") (fail "A /= A") (pass "A /= A"))
  (if (= "A" "B") (fail "A = B") (pass "A = B"))
  (if (< "A" "B") (pass "A < B") (fail "A < B"))
  (if (> "A" "B") (fail "A > B") (pass "A > B"))
  (if (/= "A" "B") (pass "A /= B") (fail "A /= B"))
  (if (= "A" "AA") (fail "A = AA") (pass "A = AA"))
  (if (< "A" "AA") (pass "A < AA") (fail "A < AA"))
  (if (> "A" "AA") (fail "A > AA") (pass "A > AA"))
  (if (/= "A" "AA") (pass "A /= AA") (fail "A /= AA")))
;
;  Test basic math functions
;
(defun test-basic-math ()
  (verify-equal 6 (+ 1 2 3) "Add positive")
  (verify-equal 0 (+ 1 2 -3) "Add mixed")
  (verify-equal -6 (+ -1 -2 -3) "Add negative")
  (verify-equal (* 2 3 4) 24 "Mul positive")
  (verify-equal (* -2 3 4) -24 "Mul mixed")
  (verify-equal 24 (* -6 -4) "Mul negative")
  (verify-equal (- 1 2 3) -4 "Sub result negative")
  (verify-equal (- 4 1) 3 "Sub positive")
  (verify-equal (- -2 -3) 1 "Sub negative")
  (verify-equal (- -2 3) -5 "Sub mixed")
  (verify-equal (/ 24 3 4) 2 "Div positive")
  (verify-equal (/ -24 3) -8 "Div mixed")
  (verify-equal (/ -24 -3) 8 "Div negative")
  (verify-equal (/ 24 5) 4 "Div inexact"))
;
;  Test global variables with setq
;
(defun test-global ()
  (setq **GLOB** 1)
  (verify-equal **GLOB** 1 "Set global variable")
  (setq **GLOB** (+ 1 **GLOB**))
  (verify-equal **GLOB** 2 "Increment global")
  (setq **GLOB** "Hello")
  (verify-equal **GLOB** "Hello" "Set global to string"))
;
;  Test local variables with setq
;
(defun test-local ()
  (local ((a (+ 1 2)) (b 4) (c 0))
    (verify-equal a 3 "A is 3")
    (verify-equal b 4 "B is 4")
    (verify-equal c 0 "C is 0")
    (setq a (+ 1 a))
    (verify-equal a 4 "A is 4")
    (setq b -10)
    (verify-equal b -10 "B is -10")
    (setq c (+ a 2))
    (verify-equal c 6 "C is 6")
    (local ((a 1))
      (verify-equal a 1 "A is 1")
      (setq a 20)
      (verify-equal a 20 "A is 20"))
    (verify-equal a 4 "A is 4")))
;
;  Test dowhile operation
;
(defun test-dowhile ()
  (local ((count 0) (accum 0))
    (dowhile (< count 10)
      (setq count (+ count 1))
      (setq accum (+ accum count))
      (print "Counter is " count ", Accumulator is " accum)
      (terpri))
    (verify-equal count 10 "Count is 10")
    (verify-equal accum 55 "Accumulator is 55")))
;
;  Test dotimes operation
;
(defun test-dotimes ()
  (local ((accum 0) result)
    (setq result (dotimes (n 10 20)
      (print "Count is " n)
      (terpri)
      (setq accum (+ accum n))))
    (verify-equal result 20 "Result returned")
    (verify-equal accum 45 "Accumulator is 45")))

