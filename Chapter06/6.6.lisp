;; ANSI Common Lisp: exercise 6.6

;; a closure
(let ((max nil))
    (defun print-max (n)
        (if (or (null max) (> n max)) 
            (setf max n)
            max)))