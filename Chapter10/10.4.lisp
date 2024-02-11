;; ANSI Common Lisp: exercise 10.4


;; recursive local function - works OK
(defmacro ntimes-l (n &rest body)
    (labels ((_ntimes (i)
        (if (= i 0)
            nil
            `(progn
                ,@body
                ,(_ntimes (- i 1))))))
    (_ntimes n)))

(let ((i 0))
    (ntimes-l 4 (format t "######~%") (incf i) (format t "i = ~A~%" i)))


;; recursive macro - works OK
(defmacro ntimes-r (n &rest body) 
    (if (= n 0)
        nil
        `(progn
            ,@body
            (ntimes-r ,(- n 1) ,@body))))

; testing
(macroexpand-1 '(ntimes-r 4 (format t "n = ~%")))

(ntimes-r 4 (format t "n = ~%") (format t "body ~%"))

(let ((i 0))
    (ntimes-r 4 (format t "######~%") (incf i) (format t "i = ~A~%" i)))


;; recursive nested function - works OK
(defmacro ntimes (n &rest body)
    (defun _ntimes (i)
        (if (= i 0)
            nil
            `(progn
                ,@body
                ,(_ntimes (- i 1)))))
    (_ntimes n))

; testing
(macroexpand-1 
    '(ntimes 3 (format t "n = ~%")))

(let ((i 0))
    (ntimes 4 (format t "######~%") (incf i) (format t "i = ~A~%" i)))