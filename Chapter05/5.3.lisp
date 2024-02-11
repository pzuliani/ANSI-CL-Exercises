;; ANSI Common Lisp: exercise 5.3

;; weird square
(defun wsq (x)
    (cond 
        ((and (>= x 1) (<= x 5)) nil)
        (t (* x x))))