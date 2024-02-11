;; ANSI Common Lisp: exercise 10.1

(setf x 'a   y 'b   z '(c d))

; (a)
`(,z ,x z)

; (b)
`(x ,y ,@z)

; (c)
`( (,@z ,x) z)