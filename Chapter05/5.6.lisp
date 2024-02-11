;; ANSI Common Lisp: exercise 5.6

;; iterative version
(defun intersperse (o lst)
    (let ((len (length lst)) (l nil))
        (if (< len 2)
            lst
            (dolist (x (subseq lst 0 (- len 1)))
                (setf l (append l (list x o)))))
        (append l (last lst))))


;; recursive version
(defun intersperse-rec (o lst)
    (let ((len (length lst)))
        (if (< len 2)
            lst
            (cons (car lst) (cons o (intersperse-rec o (cdr lst)))))))
