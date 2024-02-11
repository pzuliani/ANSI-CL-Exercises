;; ANSI Common Lisp: exercise 8.5

(defparameter *words* (make-hash-table :size 10000))

(defparameter *wl* nil)       ; the list of words in the quote

(defconstant maxword 100)

(defun read-text (pathname fun)
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
                (funcall fun (intern (string-downcase (subseq buffer 0 pos))))  ; mod
                (setf pos 0))
              (let ((p (punc c)))
                (if p (funcall fun p)))))))))     ; mod

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


;; utility function
(defun print-ht (ht)
    (maphash #'(lambda (k v) (format t "~A = ~A~%" k v)) ht))


;; add symbol to words list
(defun see-to-list (symb)
  (push symb *wl*))


;; main function: return T if quote is from text, nil otherwise
(defun check-quote (textfile quotefile)
  (let ((prev '|.|))
    (read-text textfile #'see)
    (read-text quotefile #'see-to-list)
    (dolist (word (reverse *wl*) t)         ; iterate over all the words of the quote
      (multiple-value-bind (val keyp) (gethash prev *words*)
        (if (or (not keyp) (null (assoc word val)))
          (return-from check-quote nil)   ; word is not in the text or does not follow prev, stop
          (setf prev word))))))
