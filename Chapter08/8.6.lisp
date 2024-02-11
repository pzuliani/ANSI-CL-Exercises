;; ANSI Common Lisp: exercise 8.6

(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof) 
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn
              (setf (aref buffer pos) c)
              (incf pos))
            (progn
              (unless (zerop pos)
                (see (intern (string-downcase (subseq buffer 0 pos))))
                (setf pos 0))
              (let ((p (punc c)))
                (if p (see p)))))))))
  
(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) (#\: '|:|) 
    (#\! '|!|) (#\? '|?|) ))

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))


(defun generate-text (n &optional (prev '|.|))
  (unless (zerop n)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))


(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

(defun print-ht (ht)
    (maphash #'(lambda (k v) (format t "~A = ~A~%" k v)) ht))


;; assume word is in the input text
(defun gentext-mid-word (pathname word n)
  (let ((m (if (evenp n) (/ n 2) (/ (1+ n) 2))))
    (read-text pathname)
    (generate-text m)
    (format t "~A " word)
    (generate-text (- n m) word)
    (format t "~%")))