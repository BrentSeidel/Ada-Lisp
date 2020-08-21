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
  (local ((temp (car *stack*)))
    (setq *stack* (cdr *stack*))
    temp))
