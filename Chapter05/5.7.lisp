;; ANSI Common Lisp: exercise 5.7
;; I assume that the list is processed through a sliding window of size 2

;; (a) recursive version
(defun pairdiff-rec (n lst)
    (if (< (length lst) 2)
        t
        (and (eql (abs (- (first lst) (second lst))) n) (pairdiff-rec n (cdr lst)))))


;; (b) iterative version (more efficient than the recursive one)
(defun pairdiff (n lst)
    (let ((len (length lst)))
        (if (< len 2)
            t
            (do ((x 0 (+ x 1))) ((> x (- len 2)) t)     ; return true if end of loop is reached
                (when (not (eql (abs (- (elt lst x) (elt lst (+ x 1)))) n))
                    (return-from pairdiff nil))))))


;; (c) with mapc and return (more efficient than the recursive one)
(defun pairdiff-mapc (n lst)
    (let ((len (length lst)))
        (if (< len 2)
            t
            (mapc #'(lambda (lst-a lst-b)
                (when (not (eql (abs (- lst-a lst-b)) n))
                    (return-from pairdiff-mapc nil)))
                (cdr lst) (subseq lst 0 (- len 1))))
        t))  ; if we arrive here then return true