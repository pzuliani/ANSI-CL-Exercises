;; ANSI Common Lisp: exercise 6.9

(defun myapply (fn &rest args)
    (let ((*print-base* 8))
        (princ (apply fn args))))