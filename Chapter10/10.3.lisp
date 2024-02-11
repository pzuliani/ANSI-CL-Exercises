;; ANSI Common Lisp: exercise 10.3

(defmacro nth-expr (n e &rest args)
    (let ((l `(,e ,@args)))
        (nth n l)))


; testing
(macroexpand-1 (nth-expr 1 (/ 1.0 2.0) (+ 1 2)))

(let ((i 4.0))
    (nth-expr 0 (/ 1.0 i) (+ i 2)))