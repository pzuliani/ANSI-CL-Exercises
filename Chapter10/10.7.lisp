;; ANSI Common Lisp: exercise 10.7

(defmacro mypush (obj lst)
    `(setf ,lst (cons ,obj ,lst)))


; these two calls give different results, but I am not entirely
; sure they answer the exercise correctly

(let ((lst nil))
    (mypush (car (mypush 1 lst)) (car (mypush 1 lst)))
    lst)

(let ((lst nil))
    (push (car (push 1 lst)) (car (push 1 lst)))
    lst)


(macroexpand-1 '(mypush (car (mypush 1 lst)) (car (mypush 1 lst))))

(macroexpand-1 '(push (car (push 1 lst)) (car (push 1 lst))))