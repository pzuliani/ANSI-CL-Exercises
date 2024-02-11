;; ANSI Common Lisp: exercise 5.2

(defun mystery (x y)
    (if (null y)
        nil
        (if (eql (car y) x) 
            0
            (let ((z (mystery x (cdr y))))
                (and z (+ z 1))))))

;; my code
(defun mystery2 (x y)
    (cond
        ((null y) nil) 
        ((eql (car y) x) 0) 
        (t (let ((z (mystery x (cdr y))))
                (and z (+ z 1))))))