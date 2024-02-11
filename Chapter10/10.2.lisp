;; ANSI Common Lisp: exercise 10.2

(defmacro myif (test then else)
    `(cond 
        (,test ,then)
        ((not ,test) ,else)))


; testing
(macroexpand-1 
`(myif (< x 10)
        (format t "x = ~A~%" x)
        (incf x)))

(setf x 19)
(myif (< x 10)
        (format t "x = ~A~%" x)
        (incf x))