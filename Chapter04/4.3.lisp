;; ANSI Common Lisp: exercise 4.3

;; the tree structure
(defstruct ttree 
    data (c1 nil) (c2 nil) (c3 nil))

;; create an example tree
(setf myt (make-ttree :data 'abab))

(setf (ttree-c1 myt) (make-ttree :data 'caca))

(setf (ttree-c3 myt) (make-ttree :data 'dada))

(setf (ttree-c2 (ttree-c1 myt)) (make-ttree :data 'eaea))

;; 4.3 (a)
;; copy a tree making new nodes
(defun cp-ttree (atree)
    (if (null atree)
        nil 
        (make-ttree :data (ttree-data atree) :c1 (cp-ttree (ttree-c1 atree)) :c2 (cp-ttree (ttree-c2 atree)) :c3 (cp-ttree (ttree-c3 atree)))))

;; test functionality
(setf mytc (cp-ttree myt))

;; 4.3 (b)
;; find an element in the tree using the data field
(defun find-ttree (obj atree)
    (if (null atree)
        nil 
        (if (eql obj (ttree-data atree))
            t
            (if (find-ttree obj (ttree-c1 atree))
                t 
                (if (find-ttree obj (ttree-c2 atree))
                    t 
                    (if (find-ttree obj (ttree-c3 atree))
                        t 
                        nil))))))
            

;; test functionality
(find-ttree 'caca myt)
(find-ttree 'caa myt)
(find-ttree 'dada myt)
(find-ttree 'eaea myt)