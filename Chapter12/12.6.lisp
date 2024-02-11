;; ANSI Common Lisp: exercise 12.6

(defun find-cdr-circ (obj l)
  (if (equal obj (car l))
    t
    (if (eql l (cdr l))
      nil
      (find-cdr-circ obj (cdr l)))))


; testing
(find-cdr-circ 'ey '(a b c d e))

(find-cdr-circ 'e #1='(a b c d e #1#))
(find-cdr-circ 'f #1='(a b c d e #1#))
(find-cdr-circ 'a #1='(a b c d e #1#))

(setf *print-circle* t
  l #1='(a b c d e #1#))