;; ANSI Common Lisp: exercise 4.4

;; the third argument of print-function can be usually ignored [ACL, page 70]
(defstruct (node (:print-function (lambda (n s) 
        (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

;; from the ACL book
(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                  :elt elt
                  :l   (bst-insert obj (node-l bst) <)
                  :r   (node-r bst))
                (make-node
                  :elt elt
                  :r   (bst-insert obj (node-r bst) <)
                  :l   (node-l bst)))))))

;; new code
;;;;;
;; for testing 
(setf nums nil)

(dolist (x '(4 9 1 0 -3 578 342 -45 17))
  (setf nums (bst-insert x nums #'<)))

;; reverse inorder visit (from largest to smallest)
(defun bst-revinorder (fn bst)
  (when bst
    (bst-revinorder fn (node-r bst))
    (funcall fn (node-elt bst))
    (bst-revinorder fn (node-l bst))))

;; reverse inorder, printing the elements (from largest to smallest)
(bst-revinorder #'(lambda (elt) (format t "~A " elt)) nums)

;;;;;
;; returns all the elements in a list, in descending order
(defun bst-inorder-lst (bst)
  (let ((lst nil))
    (defun bst-inorder (fn bst)
      (when bst
         (bst-inorder fn (node-l bst))
         (setf lst (funcall fn (node-elt bst)))
         (bst-inorder fn (node-r bst))))
    (bst-inorder #'(lambda (elt) (cons elt lst)) bst)
    lst))

;; testing the functionality
(bst-inorder-lst nums)