;; ANSI Common Lisp: exercise 7.3

;; strip each line of ifname of what comes after % and write it on ofname
(defun stripc (ifname ofname)
    (let ((ipath (make-pathname :name ifname)) (opath (make-pathname :name ofname)))
        (with-open-file (istr ipath :direction :input)
            (with-open-file (ostr opath :direction :output :if-exists :supersede)
                (do ((line (read-line istr nil 'eof) (read-line istr nil 'eof))) ((eql line 'eof))
                    (format ostr "~A~%" (subseq line 0 (position #\% line))))))))

(stripc "test.lisp" "out.lisp")