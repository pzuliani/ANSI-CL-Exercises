;; ANSI Common Lisp: exercise 10.8

(defmacro mydouble (x)
    `(* 2 ,x))

(macroexpand-1 '(mydouble x))

(let ((x 23))
    (mydouble x))