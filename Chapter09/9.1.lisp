;; ANSI Common Lisp: exercise 9.1


(defun non-dec (l)
    (multiple-value-call #'<= (values-list l)))     ; values-list is an 'inverse' of list