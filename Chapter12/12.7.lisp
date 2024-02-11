;; ANSI Common Lisp: exercise 12.7

;; inefficient and recursive, but working
(defun is-cdr-circ (l tail)
  (cond ((eql l tail) t)
    ((null tail) nil) 
    (t (is-cdr-circ l (cdr tail)))))


;; iterative version
(defun is-cdr-circ-i (l)
  (do ((tail (cdr l) (cdr tail))) (())    ; no termination condition
    (cond ((eql l tail) (return-from is-cdr-circ-i t))
          ((null tail) (return-from is-cdr-circ-i nil)))))


; testing
(let ((l (list 'a 'b 'c 'd 'e)))
  (is-cdr-circ l (cdr l)))

(let ((l (list 'a 'b 'c 'd 'e)))
  (is-cdr-circ-i l))

(progn
  (setf *print-circle* t
    l (list 'a 'b 'c 'd) 
    (cdr (last l)) l)
  ;(is-cdr-circ l (cdr l))
  (is-cdr-circ-i l))

