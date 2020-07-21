;
;  Test functions for the Tiny Lisp Interpreter
;
;
;  Support functions
;
; Prints a pass message
(defun pass (n)
  (print "PASS: " n)
  (new-line))
; Prints a fail message
(defun fail (n)
  (print "***FAIL: " n)
  (new-line))
;  If two values are equal, print pass message, otherwise print fail message
(defun verify-equal (a b msg)
  (if (= a b) (pass msg) (fail msg)))
;
;  Test if and booleans
;
;  Note that there is currently a bug where defun does not work for functions
;  with no parameters.
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
;  Testing the do while loop
;
(setq var 0)
(dowhile (< var 100) (print "Var is " var) (new-line) (setq var (+ var 1)))

(setq var1 0)
(setq var2 0)
(dowhile (< var1 100)
  (setq var2 0)
  (print "Var1 is " var1)
  (new-line)
  (setq var1 (+ 1 var1))
  (dowhile (< var2 100)
    (print "  Var2 is " var2)
    (setq var2 (+ 1 var2))
    (new-line)))

(dowhile (< var1 5)
  (setq var2 0)
  (setq var1 (+ 1 var1))
  (dowhile (< var2 5)
    (setq var2 (+ 1 var2))))

(defun hello (var) ((print "Hello " var) (new-line)))

(defun range (val upper lower)
   (if (< val upper)
      (if (> val lower)
        (print "In range")
        (print "Below range"))
      (print "Above range"))
     (new-line))

(defun fact (n)
   (if (> n 1)
    (* n (fact (- n 1)))
    1))

(defun abs (n)
  (if (> 0 n)
    (- 0 n)
    (+ 0 n)))

(defun hello (n)
  (setq var (abs n))
  (dowhile (< 0 var)
    (print "Hello #" var)
    (new-line)
    (setq var (- var 1))))
;
;  Rewriten to use dotimes
;
(defun hello (n)
  (dotimes (var n)
    (print "Hello #" var)
    (new-line)))
;
;  This should give a workout for recursive functions.  The values returned
;  should be:
;  (fib 1) => 1
;  (fib 2) => 2
;  (fib 3) => 3
;  (fib 4) => 5
;  (fib 5) => 8
;
(defun fib (n)
  (if (< n 2)
    1
    (+ (fib (- n 2)) (fib (- n 1)))))

(defun test (n)
  (setq n (+ 3 1))
  (print "N is " n))

