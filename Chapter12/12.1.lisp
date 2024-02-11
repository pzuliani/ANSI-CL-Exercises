;; ANSI Common Lisp: exercise 12.1

; three conses each containing (a)
(let ((l (list '(a) '(a) '(a))))
    (format t "l = ~A~%" l)
    (eql (first l) (second l)))     ; check that the conses are different objects

; there is only one cons for (a) 
(let* ((elt '(a))
    (l (list elt elt elt)))
    (format t "l = ~A~%" l)
    (and (eql (first l) (second l))
        (eql (second l) (third l))))     ; check that the conses are the same object

; two conses each containing symbol a 
(let* ((elt '(a))
    (l (list elt elt '(a))))
    (format t "l = ~A~%" l)
    (eql (second l) (third l)))     ; these two conses are different objects

;; See also https://www.shido.info/lisp/pacl2_e.html
