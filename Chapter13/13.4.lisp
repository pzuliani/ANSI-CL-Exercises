
;; ANSI Common Lisp: exercise 13.4

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (time (bfs end
                   ; nconc is a destructive version of append
                   (nconc (cdr queue) (new-paths path node net))    
                   ;(append (cdr queue) (new-paths path node net))
                   net)))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(setf min '((a b c) (b c) (c d e f) (d b) (f h)))

(shortest-path 'a 'h min)