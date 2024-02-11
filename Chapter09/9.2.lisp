;; ANSI Common Lisp: exercise 9.2


; returns a list
; n = number of cents
; l = list of coins size
(defun coins (n l)
    (if (or (null l) (= n 0))
        nil
        (let* ((c (floor n (car l)))        ; number of coins of current size
            (rem (- n (* c (car l)))))      ; remaining sum to be split
            (cons c (coins rem (cdr l))))))


; convert list to values
(values-list (coins 147 '(25 10 5 1)))      ; dollar coins
(values-list (coins 147 '(50 20 10 5 1)))   ; euro coins
