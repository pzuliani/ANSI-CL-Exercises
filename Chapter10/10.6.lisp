;; ANSI Common Lisp: exercise 10.6


(defmacro save-vars (vars &body body)
  `(let* ((s (gensym)) (s nil)) 
    ,@(mapcar #'(lambda (v) `(push ,v s)) vars)
    ,@body
    ,@(mapcar #'(lambda (v) `(setf ,v (pop s))) (reverse vars))))


; testing
(macroexpand-1 '(save-vars (a b c) (format t "In the body a, b, c = ~A, ~A, ~A~%" (incf a) (incf b) (incf c))))

(let ((a 0) (b 1) (c 2))
  (save-vars (a b c) (format t "In the body a, b, c = ~A, ~A, ~A~%" (incf a) (incf b) (incf c)))
  (format t "a, b, c = ~A, ~A, ~A~%" a b c))
