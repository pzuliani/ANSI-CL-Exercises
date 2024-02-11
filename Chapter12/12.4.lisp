;; ANSI Common Lisp: exercise 12.4

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q) 
  (pop (car q)))

(defun jump-queue (obj q)
  (let ((l (list obj)))
    (setf (cdr l) (car q)
        (car q) l)))

;; Testing
(setf q1 (make-queue))
(progn (enqueue 'a q1)
  (enqueue 'b q1)
  (enqueue 'c q1)
  (jump-queue 'd q1))