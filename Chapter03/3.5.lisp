;; ANSI Common Lisp: exercise 3.5

;; recursive
(defun pos+r (lst)
    (if (eql lst nil)
        nil
        (cons (+ 1 (car lst)) (pos+r (cdr lst))))) ; if the second argument of cons is a list, then it is appended to the first argument

;; iterative
(defun pos+i (lst)
    (let ((l nil))
        (dolist (obj lst)
            (setf l (cons (+ 1 obj) l)))  ; the new cons is added to the head of l
        (reverse l)))

;; iterative - 2nd version (without reversing the list)
(defun pos+i2 (lst)
    (let ((l nil))
        (dolist (obj lst)
           (setf l (append l (list (+ 1 obj)))))
    l))

;; mapcar version
(defun pos+m (lst)
    (mapcar #'(lambda (x) (+ x 1)) lst))    ; amazing!