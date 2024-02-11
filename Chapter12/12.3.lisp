;; ANSI Common Lisp: exercise 12.3

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q) 
  (pop (car q)))

(defun copy-queue (q)
  (let ((nq (make-queue)))
    (setf (car nq) (copy-tree (car q))
        (cdr nq) (last (car nq)))
    nq))

;; Testing
(setf q1 (make-queue))
(progn (enqueue 'a q1)
  (enqueue 'b q1)
  (enqueue 'c q1))
(setf q2 (copy-queue q1))