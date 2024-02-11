;; ANSI Common Lisp: exercise 5.1

;; 5.1(a)
(defun ex_a (y)
    (let ((x (car y)))
        (cons x x)))

;; my code for (a)
(defun ex_aa (y)
    (progn 
        (setf x (car y))
        (cons x x)))


;; 5.1(b)
(defun ex_b (x z)
    (let* ((w (car x)) (y (+ w z)))
        (cons w y)))

;; my code for (b)
(defun ex_bb (x z)
    (progn
        (setf w (car x)) 
        (setf y (+ w z))
        (cons w y)))
