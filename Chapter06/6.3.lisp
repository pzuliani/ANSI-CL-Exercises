;; ANSI Common Lisp: exercise 6.3

(defun cnt-args (&rest args)
    (length args))

(cnt-args 1 4 5 #\a "ciao" #'eql)