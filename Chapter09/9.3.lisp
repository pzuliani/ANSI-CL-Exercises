;; ANSI Common Lisp: exercise 9.3


(defun simulate-contest (years)
    (let ((a (make-array years :initial-element 0)))
        (dotimes (i years)
            (setf a (map-into a #'(lambda (v) (+ v (random 2))) a)))
        (format t "Wigglies: ~A~%" a)
        (setf a (map-into a #'(lambda (v) (- 10 v)) a))
        (format t "Wobblies: ~A~%" a)))

(simulate-contest 10)