;; ANSI Common Lisp: exercise 5.8

(defun maxminl (lst)
    (if (< (length lst) 2)
        (values (car lst) (car lst))
        (multiple-value-bind (pmin pmax) (maxminl (cdr lst))
            (values (min (car lst) pmin) (max (car lst) pmax)))))

;; testing (alive repl only prints the first returned value)
(multiple-value-list (maxminl '(101 1 8 102 2 4 1 -4 90)))

;; testing
(maxminl '(101 1 8 102 2 4 1 -4 90))