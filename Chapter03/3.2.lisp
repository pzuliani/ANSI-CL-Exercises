;; ANSI Common Lisp: exercise 3.2

;; Does x come before y in the dictionary?
(defun dict< (x y)
    ;; A dictionary of symbols
    (let ((dict '(a b c d e f g h i j k l m n o p q r s t v w x y z)))
        (> (length (member x dict)) (length (member y dict)))))

(defun new-union (x y)
    (sort (union x y) #'dict<))

(new-union '(a f e b c) '(b z e  r w a d))
