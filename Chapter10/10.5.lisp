;; ANSI Common Lisp: exercise 10.5

;; utility from ANSI CL
;; I have deleted the h variable as not needed
(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
           ((>= ,g ,n))
         ,@body)))

;; utility from ANSI CL
(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))


(defmacro n-of (n expr) 
  `(let ((lst nil))
    (ntimes ,n (append1f lst ,expr)) 
    lst)) 

; testing
(macroexpand-1 '(n-of 6 (incf i)))

(let ((n 20))
  (n-of 6 (progn (incf n) (incf n))))      

(macroexpand-1
  '(let ((n 20))
    (n-of 6 (progn (incf n) (incf n))))) 

