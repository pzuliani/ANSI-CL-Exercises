;; ANSI Common Lisp: exercise 3.8

;; ugly, but working
(defun showdots (lst)
    (dolist (obj lst) 
        (format t "(~A . " obj))
    (format t "NIL")
    (dolist (obj lst) 
        (format t ")")))

;; cryptic version!
(defun sdots (lst)
    ;; does not print the dot '.' if on the last element of lst
    ;; ~:* does not consume the argument unless we are at the last repetition 
   (format t "(~{~A~^ . (~} . NIL~v@{~c~:*~} ~%" lst (length lst) #\)))

