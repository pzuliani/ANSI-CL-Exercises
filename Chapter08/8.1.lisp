;; ANSI Common Lisp: exercise 8.1

;; From https://franz.com/support/documentation/current/ansicl/dictentr/eq.htm
;; "An implementation is permitted to make "copies" of characters and numbers at any time. 
;; The effect is that Common Lisp makes no guarantee that eq is true even when both its arguments 
;; are "the same thing" if that thing is a character or number."

;; how to create two symbols with the same name, but not eql
'asymbol

; creates a new symbol with the same string name
(setf nsym (copy-symbol 'asymbol))  

; check that the name is the same
(symbol-name 'asymbol)
(symbol-name nsym)

; check that they are not eql
(eql 'asymbol nsym)




;; same exercise, but with two packages
(setf p1 (defpackage "myapp1" (:use "COMMON-LISP")))

(setf p2 (defpackage "MYAPP1" (:use "COMMON-LISP")))

(intern "ciao" "myapp1")

(intern "ciao" "MYAPP1")

;; run this function in a 'plain' sbcl REPL to see all the output
(multiple-value-bind (s1 s2) (intern "ciao" "myapp1")
    (format t "~A ~A ~%" s1 s2)
    (multiple-value-bind (t1 t2) (intern "ciao" "MYAPP1")
        (format t "~A ~A ~%" t1 t2)
        ; they have the same name
        (format t "~A ~A~%" (symbol-name s1) (symbol-plist s1))
        (format t "~A ~A~%" (symbol-name t1) (symbol-plist t1))
        (format t "s1 =? t1 ~A~%" (equal s1 t1))))          ; they are not the same symbol
