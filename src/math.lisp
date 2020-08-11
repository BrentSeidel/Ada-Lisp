;
;  A collection of simple math functions using the tiny Lisp interpreter
;
;
;  Recursive function to compute factoral of a number.
;
(defun fact (n)
   (if (> n 1)
    (* n (fact (- n 1)))
    1))
;
;  Return the absolute value of a number
(defun abs (n)
  (if (> 0 n)
    (- 0 n)
    (+ 0 n)))
;
;  Recursive function to compute Fibonacci numbers.
;
;  This should give a workout for recursive functions.  The first few values
;  returned should be:
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
;
;  Iterative Fibonacci function
;
(defun fibi (n)
  (local (temp (n1 0) (n2 1))
    (dotimes (iter n)
      (setq temp (+ n1 n2))
      (setq n1 n2)
      (setq n2 temp))
    n2))
;
;  Compute the remainder
;
(defun rem (a b) (- a (* (/ a b) b)))
;
;  Squares and square roots
;
(defun sqr (n) (* n n))
;
;  Integer square root of n.  Returns i where i*i <= n and (i+1)*(i+1) > n.
;  This assumes that n > 0.  Note that this is not a particularly efficient
;  algorithm.
;
(defun sqrt (n)
  (if (< n 1)
    0
    (if (< n 4)
      1
      (local ((temp 2))
        (dowhile (< (* temp temp) (+ n 1))
          (setq temp (+ temp 1)))
        (- temp 1)))))
;
;  Compute integer square root of n using bisection algorithm.  This is not the
;  best, but will be better than the one above that just counts up.
;
(defun sqrt (n)
  (if (< n 1)
    0
    (if (< n 4)
      1
      (if (< n 9)
        3
        (local ((min 1) (max (/ n 2)) (mid 0))
          (dowhile (> (- max min) 1)
            (setq mid (/ (+ min max) 2))
            (if (> (* mid mid) n)
              (setq max mid)
              (setq min mid)))
           (+ 0 min))))))

