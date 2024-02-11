;; ANSI Common Lisp: exercise 12.5

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q) 
  (pop (car q)))

(defun mv-obj-queue (obj q)
  (let ((i (position obj (car q))))
    (cond ((null i) nil) ; return nil if obj is not in the queue
        ((zerop i) q)   ; obj is already at the front
        ((= i (- (length (car q)) 1))    ; obj is at the end of the queue
          (let* ((prec (nthcdr (- i 1) (car q)))
                (elt (cdr prec)))
            (setf (cdr elt) (car q)
              (car q) elt
              (cdr q) prec
              (cdr prec) nil)))
        (t (let* ((prec (nthcdr (- i 1) (car q))) ; obj is somewhere else
                (elt (cdr prec)))
              (setf (cdr prec) (cdr elt)     
                (cdr elt) (car q)
                (car q) elt))))))

;; Testing
(let ((q (make-queue)))
  (enqueue 'a q)
  (enqueue 'b q)
  (enqueue 'c q)
  (enqueue 'K q)
  (enqueue 'c q)
  (mv-obj-queue 'c q) 
  q)

(let ((q (make-queue)))
  (enqueue 'a q)
  (enqueue 'b q)
  (enqueue 'h q)
  (enqueue 'K q)
  (enqueue 'c q)
  (mv-obj-queue 'c q) 
  q)