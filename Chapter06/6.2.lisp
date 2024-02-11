;; ANSI Common Lisp: exercise 6.2

;; returns nil if vec empty
(defun bin-search (obj vec &optional (test #'eql) (start 0) (end (- (length vec) 1)) (key #'identity))
    (if (>= end start)
        (finder obj vec test start end key)
        nil))

(defun finder (obj vec test start end key)
  (let ((range (- end start)))
    (if (zerop range)
        (if (funcall test (funcall key obj) (funcall key (aref vec start)))         ; calling test 
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec test start (- mid 1) key)
                (if (> obj obj2)
                    (finder obj vec test (+ mid 1) end key)
                    obj)))))))

