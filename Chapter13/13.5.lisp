
;; ANSI Common Lisp: exercise 13.5

;; this code does not create a new tree every time an element is inserted


(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defparameter pool (make-array 1000 :fill-pointer t))

(dotimes (i 1000) 
  (setf (aref pool i) (make-node)))

(defun bst-insert (obj bst <)
  (bst-insert-pool obj bst nil nil <))    ; tree is empty initially, so prev and dir are nil


; helper function for bst-insert
(defun bst-insert-pool (obj bst prev dir <)
  (if (null bst)
      (let ((n (vector-pop pool)))
        (setf (node-elt n) obj)                       ; store obj in new node
        (cond ((eql dir 'l) (setf (node-l prev) n))   ; I am the left child of prev
          ((eql dir 'r) (setf (node-r prev) n))       ; I am the right child of prev
          ((null dir) ()))
        n)
      (let ((elt (node-elt bst))) 
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
              (bst-insert-pool obj (node-l bst) bst 'l <)
              (bst-insert-pool obj (node-r bst) bst 'r <))))))


(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))


; removes obj and returns the root of the tree
(defun bst-remove (obj bst <)
  (if (null bst)
    nil
    (if (eql obj (node-elt bst))  ; are we deleting the root node?
      (percolate-root bst)     ; returns the new root
      (progn 
        (bst-remove-pool obj bst nil nil <)
        bst))))                ; the root hasn't changed


; for deleting the root node only
(defun percolate-root (bst)
  (let ((l (node-l bst)) (r (node-r bst)) (ret nil))
    (cond ((null l) (setf ret r))
          ((null r) (setf ret l))
          (t (if (zerop (random 2))
            (progn                          ; move the right subtree
              (setf (node-l (bst-min r)) l)
              (setf ret r))
            (progn                          ; move the left subtree
              (setf (node-r (bst-max l)) r) 
              (setf ret l)))))
    (setf (node-elt bst) nil (node-l bst) nil (node-r bst) nil) ; clear the node before pushing it back
    (vector-push bst pool)
    ret))


; helper function for bst-remove
(defun bst-remove-pool (obj bst prev dir <)
  (if (null bst)
      nil 
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst prev dir)
            (if (funcall < obj elt)
                (bst-remove-pool obj (node-l bst) bst 'l <)
                (bst-remove-pool obj (node-r bst) bst 'r <))))))


; helper function for bst-remove-pool
; assumes bst is not the root node (so prev is never nil)
(defun percolate (bst prev dir)
  (let ((l (node-l bst)) (r (node-r bst)) (ret nil))
    (cond ((null l) 
            (if (eql dir 'l)
              (setf (node-l prev) r)
              (setf (node-r prev) r))
            (setf ret r))
          ((null r)
            (if (eql dir 'r)
                  (setf (node-r prev) l)
                  (setf (node-l prev) l))
            (setf ret l))
          (t (if (zerop (random 2))   
            (progn
              (if (eql dir 'l)         ; move right subtree 
                (setf (node-l prev) r)
                (setf (node-r prev) r))
              (setf (node-l (bst-min r)) l)
              (setf ret r)))
            (progn
              (if (eql dir 'r)        ; else move left subtree
                (setf (node-r prev) l)
                (setf (node-l prev) l))
              (setf (node-r (bst-max l)) r) 
              (setf ret l))))
    (setf (node-elt bst) nil (node-l bst) nil (node-r bst) nil) ; clear the node before pushing it back
    (vector-push bst pool)
    ret))


;; testing
(setf nums nil)
(format t "pool is ~A~%" (length pool))
(setf nums (bst-insert 200 nums #'<))
(dolist (x '(300 180 185 184 10 220 310 308 320))
    (bst-insert x nums #'<))
(bst-insert 225 nums #'<)
(bst-insert -100 nums #'<)

;(setf newroot (bst-remove 200 nums #'<))
;(unless (eq newroot nums)
;  (format t "new root!!~%")
;  (setf nums newroot))