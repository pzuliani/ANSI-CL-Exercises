;; ANSI Common Lisp: exercise 4.5

(setf itflag (make-hash-table))

(setf (gethash 'colore_sx itflag) 'verde
    (gethash 'colore_c itflag) 'bianco
    (gethash 'colore_dx itflag) 'rosso)

;; this definition does not work
;;(setf itflag_l '((colore_sx . 'verde)
 ;;   (colore_dx . 'rosso)
  ;;  (colore_c . 'bianco)))

;; this is OK
(setf itflag_l (list (cons 'colore_dx  'rosso)
    (cons 'colore_sx  'verde)
    (cons 'colore_c  'bianco)))

;; from assoc-list to hash table
(defun assoc-to-ht (lst)
    (let ((ht (make-hash-table)))
        (mapcar #'(lambda (obj) (setf (gethash (car obj) ht) (cdr obj))) lst)
        ht))

;; from hash table to assoc-list
(defun ht-to-assoc (ht)
    (let ((lst nil))
        (maphash #'(lambda (key value) (setf lst (append (list (cons key value)) lst))) ht)
        lst))

;; some testing
(ht-to-assoc (assoc-to-ht itflag_l))

(ht-to-assoc itflag)