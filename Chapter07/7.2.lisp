;; ANSI Common Lisp: exercise 7.2

(defun gets (fname)
    (let ((path (make-pathname :name fname)) (acc nil))
        (with-open-file (str path :direction :input)
            (do ((s (read str nil) (read str nil))) ((null s))
                (push s acc))) acc))

(reverse (gets "test.lisp"))

;; same function, but with error handling
(defun gets-noerr (fname)
    (let ((path (make-pathname :name fname)) (acc nil))
        (multiple-value-bind (val condi)    ; get the outputs of ignore-errors
            (ignore-errors 
                (with-open-file (str path :direction :input)
                    (do ((s (read str nil) (read str nil))) ((null s))
                        (push s acc))))     ; push expression onto a stack
            (if (null condi)
                acc         ; all went well - return the list of expressions
                (format t "Error: ~A~%" condi)))))  ; something went wrong

(gets-noerr "test.lisp")