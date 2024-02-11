;; ANSI Common Lisp: exercise 3.3

;; counts the occurrences of e in l
(defun countel (e l)
    (- (length l) (length (remove e l))))

;; unsorted
(defun occurrences-unsorted (l)
    (let ((e (first l)))
        (if (eql l nil)
            nil
            (append (list (cons e (countel e l))) (occurrences (remove e l))) ; the call to list is necessary because append wants lists (but cons returns a dotted list)
        )
    )
)

;; is cons x smaller than cons y? The choice is based comparing the cdrs 
(defun cons< (x y)
    (< (cdr x) (cdr y)))

;; is cons x larger than cons y? The choice is based comparing the cdrs 
(defun cons> (x y)
    (> (cdr x) (cdr y)))

;; sorted version
(defun occurrences (l)
    (sort (occurrences-unsorted l) #'cons>))


(occurrences '(a a a b c a f c z c))