
;; ANSI Common Lisp: exercise 12.8

(defun is-car-circ (l)
    (let ((le (last l)))
        (eql (car le) le)))


;; testing
(is-car-circ '(a b c d))

(let* ((y (list 'a 'b 'c))
        (le (last y)))
    (setf (car le) le)
    (is-car-circ y))