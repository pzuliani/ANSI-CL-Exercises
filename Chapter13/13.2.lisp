
;; ANSI Common Lisp: exercise 13.2

(defun foo (x)
    (if (zerop x)
        0
        (+ 1 (foo (1- x)))))


(defun foo/tr (x sum)
    (if (zerop x)
        sum
        (foo/tr (1- x) (1+ sum))))