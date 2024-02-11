;; ANSI Common Lisp: exercise 9.5

;; returns an approximation of the first derivative of f at x
(defun approx-derive (f x)
    (let* ((h 0.001) (num (-(funcall f (+ x h)) (funcall f x))))
        (/ num h)))


;; returns a root such that f(root) < epsilon
(defun find-root (f min max epsilon)
    (let ((i (- max (/ (- max min) 2))))
        (do ((x i (- x (/ (funcall f x) (approx-derive f x))))) 
                ((< (abs (funcall f x)) epsilon) x))))

;; testing
(find-root #'(lambda (x) (- x 3.4)) -2.0 4.0 0.0001)

(find-root #'(lambda (x) (- (* x x) 3.4)) -2.0 4.0 0.0001)
