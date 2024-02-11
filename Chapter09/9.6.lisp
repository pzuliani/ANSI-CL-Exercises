;; ANSI Common Lisp: exercise 9.6


;; a recursive solution
(defun horner (x &rest n)
    (defun H (x n)
        (if (= (length n) 1)
            (car n)
            (+ (* x (H x (cdr n))) (car n))))
    (H x (reverse n)))


;; a macro (from Fateman et al. "Fast Floating-Point Processing in Common Lisp"
;; ACM Trans. Math. Soft. 21(1), 1995)
(defmacro horner-m (x n)
    (if (null (cdr n))
            (car n)
            `(+ (* ,x (horner-m ,x ,(cdr n))) ,(car n))))


;; testing
(horner 3 8 4 2 2)

(horner-m 3 (2 2 4 8))


(macroexpand-1 (horner-m 3 (2 2 4 8)))