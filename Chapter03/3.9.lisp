;; ANSI Common Lisp: exercise 3.9

;; returns the starting nodes of the graph 
;; it assumes the graph is given as a list of edges from a node (see Section 3.15)
(defun get-nodes (net)
  (if (null net)
    nil
    (cons (list (caar net)) (get-nodes (cdr net))))) ; get the first element of each edge


(defun longest-path (net)
  (let ((epaths nil)) 
    (bfs (get-nodes net) net epaths)))


;; returns two lists of simple paths by extending path by one edge:
;; 1. paths which may be extended
;; 2. paths which cannot be extended anymore
(defun new-paths (path node net)
  (let ((neighbors (cdr (assoc node net)))
      (epaths nil))            ; extendable paths
      (dolist (n neighbors)
        (if (not (member n path))
          (setf epaths (append epaths (list (cons n path)))))) ; add edge only if node has not been visited already
      (list epaths (if (null epaths) path nil))))   ; if epaths is nil then path might have maximal length


;; queue  = list of paths to extend
;; net    = graph
;; epaths = list of explored paths (nil initially)
(defun bfs (queue net epaths)
  (if (null queue)
      epaths
      ; process one path in the queue at a time
      (let ((path (car queue)))
        (let ((node (car path)))
          (let ((pathlist (new-paths path node net)))   ; try to extend the path
            ;;(format t "queue = ~A~%" queue)
            ;;(format t "pathlist = ~A~%" pathlist)
            (bfs (append (cdr queue) (car pathlist)) net (cdr pathlist)))))))


(setf net '((a b c a) (b c d) (c a) (e g) (g h g) (h f l e) (f t i a) (t y) (k z)))

(setf grafoale '((a l) (l i) (i d) (d g) (g b) (b m) (m n) (n f) (f o) (o p) (p e) (e h) (c)))
