;; ANSI Common Lisp: exercise 7.1

(defun getlines (ifname)
    (let ((ipath (make-pathname :name ifname)) (acc nil))
        (with-open-file (istr ipath :direction :input)
            (do ((line (read-line istr nil 'eof) (read-line istr nil 'eof))) ((eql line 'eof))
                (push line acc))) acc))     ; push line onto a stack

(getlines "test.lisp")