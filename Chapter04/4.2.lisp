;; ANSI Common Lisp: exercise 4.2

(defun mycopy-list (lst)
    (reduce #'cons lst :from-end t :initial-value nil))

(mycopy-list '(a b a c d e f g h))

(defun myreverse (lst)
    (reduce #'(lambda (a b) (cons b a)) lst :from-end nil :initial-value nil))

(myreverse '(a b c d e f g))
