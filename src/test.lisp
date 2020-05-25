;
;  Some test lisp commands
;
(setq t1 1)
(setq t2 2)
(setq t3 3)
(setq t4 4)
(if (/= t1 t2) (* t3 t3) (* t4 t4))
(if (= t1 t2) (* t3 t3) (* t4 t4))
;
;  Testing the do while loop
;
(setq var 0)
(dowhile (< var 100) (print "Var is " var) (new-line) (setq var (+ var 1)))

(setq var1 0)
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
   ((if (< val upper)
      (if (> val lower)
        (print "In range")
        (print "Below range"))
      (print "Above range"))
     (new-line)))

(defun fact (n)
  ((print "Computing factorial of " n)
   (new-line)
   (if (> n 1)
    (* n (fact (- n 1)))
    1)))

(defun abs (n)
  (if (> 0 n)
    (- 0 n)
    (+ 0 n)))
;
;  Testing pin commands on the Arduino Due
;
(pin-mode 10 0)
(pin-mode 11 1)
(pin-mode 12 1)
(dowhile (= (read-pin 10) (+ 0 1))
  (set-pin 11 0)
  (set-pin 12 1)
  (set-pin 11 1)
  (set-pin 12 0))
;
;
(setq pin10 (read-pin 10))
(dowhile (= pin10 1)
  (set-pin 11 0)
  (set-pin 12 1)
  (set-pin 11 1)
  (set-pin 12 0)
  (setq pin10 (read-pin 10)))

