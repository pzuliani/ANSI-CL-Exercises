;; ANSI Common Lisp: exercise 6.7

;; a closure
(let ((prev nil))
    (defun print-prev (n)
        (cond 
            ((or (null prev) (<= n prev)) (setf prev n) nil)    ; set prev and return nil (CL evaluates from left to right!)
            (t (setf prev n) t))))                              ; set prev and return T