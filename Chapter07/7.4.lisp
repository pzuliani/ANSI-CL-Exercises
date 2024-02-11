;; ANSI Common Lisp: exercise 7.4

;(format t "~{~10,2,0,'*,' F~}~%" l))   ; l must be a list!

(setf M #2a ((0.1 112.0 -89.488) (0.873 23.874 3.1458)))

;; simple, but working
(defun print-table (M)
    (let ((rows (first (array-dimensions M))) (cols (second (array-dimensions M))))
        (do ((i 0 (+ i 1))) ((>= i rows))
            (do ((j 0 (+ j 1))) ((>= j cols))
                (format t "~10,2,0,'*,' F" (aref M i j)))
            (format t "~%"))))

(print-table M)
