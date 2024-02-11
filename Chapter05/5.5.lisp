;; ANSI Common Lisp: exercise 5.5

;; iterative version
(defun precedes (c s)
    (let ((len (length s)))
        (if (< len 2) 
            nil 
            (let ((lst nil))
                (dotimes (pos (- len 1))
                    (when (eql c (char s (+ pos 1)))
                        (setf lst (append (list (char s pos)) lst)))) 
            (remove-duplicates lst)))))


;; recursive version (no removal of duplicates)
(defun precedes-rec (c s)
    (if (< (length s) 2)
        nil 
        (let ((pos (position c s :start 1)))
            (if (null pos)
                nil
                (cons (char s (- pos 1)) (precedes c (subseq s pos)))))))