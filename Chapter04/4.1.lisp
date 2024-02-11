;; ANSI Common Lisp: exercise 4.1

(defun quarter-rot (M)
    (let ((rows (first (array-dimensions M)))
        (cols (second (array-dimensions M))))
       (if (not (eql rows cols))
            (format t "Matrix is not square: rows = ~A  cols = ~A ~%" rows cols) 
            (do ((rotM (make-array (list rows cols))) (i 0 (+ i 1)))
                ((>= i rows) rotM)
                (do ((j 0 (+ j 1))) ((>= j cols))
                    (setf (aref rotM i j) (aref M (- rows j 1) i)))))))

(quarter-rot #2a((a b) (c d)))

(quarter-rot #2a((a b c) (d e f) (g h i)))

(quarter-rot #2a((a b c d) (e f g h) (i l m n) (o p q r)))
