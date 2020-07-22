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

