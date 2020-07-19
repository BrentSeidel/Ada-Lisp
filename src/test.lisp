;
;  Test functions for the Tiny Lisp Interpreter
;
(defun pass (n)
  (print "PASS: " n)
  (new-line))
(defun fail (n)
  (print "***FAIL: " n)
  (new-line))
;
(defun test-bool (a)
  (if t (pass "T literal works")
        (fail "T literal does not work"))
  (if nil (fail "NIL literal does not work")
          (pass "NIL literal works")))
;
(defun test-int-cmp (a)
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
(defun test-str-cmp (a)
  (if (= "A" "A") (pass "A = A") (fail "A = A"))
  (if (< "A" "A") (fail "A < A") (pass "A < A"))
  (if (> "A" "A") (fail "A > A") (pass "A > A"))
  (if (/= "A" "A") (fail "A /= A") (pass "A /= A"))
  (if (= "A" "B") (fail "A = B") (pass "A = B"))
  (if (< "A" "B") (pass "A < B") (fail "A < B"))
  (if (> "A" "B") (fail "A > B") (pass "A > B"))
  (if (/= "A" "B") (pass "A /= B") (fail "A /= B"))
)
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

