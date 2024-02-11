;; ANSI Common Lisp: exercise 6.8


(defun expensive (n)
    ;(format t "calling expensive with n = ~A" n)
    (* n n))

(let ((ht nil))
    (setf ht (make-hash-table :size 101))
    (defun frugal (n) 
        (multiple-value-bind (val has-key) (gethash n ht)
            (if has-key
                val
                (setf (gethash n ht) (expensive n))))))