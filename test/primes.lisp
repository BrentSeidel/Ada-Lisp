;
;  Test script to profile the Lisp interpreter using prime number
;  generation.
;
;
;  Compute the remainder
;
(defun rem (a b) (- a (* (/ a b) b)))
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
        (let ((min 1) (max (/ n 2)) (mid 0))
          (dowhile (> (- max min) 1)
            (setq mid (/ (+ min max) 2))
            (if (> mid (/ n mid))
              (setq max mid)
              (setq min mid)))
           (+ 0 min))))))
;
;  Test if a number is prime.  This depends on the functions rem and one of the
;  sqrt functions defined above.
;
(defun primep (n)
  (let ((prime T) (count 3) (limit (+ 1 (sqrt n))))
    (if (= 0 (rem n 2))
      (setq prime NIL))
    (dowhile (and prime (< count limit))
      (if (= 0 (rem n count))
        (setq prime NIL))
      (setq count (+ 2 count)))
    prime))
;
;  List prime numbers from 1 through n
;
(defun primes (n)
  (let ((value 0) (is-prime NIL) (limit (+ 1 (/ n 2))))
  (dotimes (x limit)
    (setq value (+ 1 (* 2 x)))
    (setq is-prime (primep value))
    (if is-prime (progn (print value " is prime") (terpri))))))
;
;  Compute prime numbers up to #xFFFFFF
;
(primes #xFFFFFF)
(exit)
