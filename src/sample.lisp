;
;  Initialize stack
;
; (defvar *stack* ())
;
(defun stack-init ()
  (setq *stack* ()))
;
;  Add an element to the stack
;
(defun stack-push (a)
  (setq *stack* (cons a *stack*)))
;
;  Remove an item from the stack and return it
;
(defun stack-pop ()
  (let ((temp (car *stack*)))
    (setq *stack* (cdr *stack*))
    temp))
;
;  Function that needs a lambda
;
(defun op (a) (a 2 3))

(op (lambda (a b) (+ a b)))
;
;  Interactions between functions and let variables
;
(let ((a 10)) (defun test (b) (print "Sum is " (+ a b)) (terpri))
  (test 5))

(test 6)

(let ((a 20)) (test 7))

(let ((b 30)) (test 8))
