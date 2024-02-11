;; ANSI Common Lisp: exercise 5.9

;; exercise (a) only

(defun shortest-path (start end net)
  (catch 'found       ; mod here
    (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end (append (cdr queue) (new-paths path node end net)) net))))))

; added end node to the parameters list so we can check when we are done
(defun new-paths (path node end net)    
  (mapcar #'(lambda (n) 
      (when (eql n end)                   ; are we done?
          (throw 'found (cons n path)))   ; found it, stop searching
      (cons n path))            ; previous code 
      (cdr (assoc node net))))  ; previous code

(setf net '((a b c d) (b c) (c d e f)))

(shortest-path 'a 'f net)