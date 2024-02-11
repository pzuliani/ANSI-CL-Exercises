;; ANSI Common Lisp: exercise 6.4

(defun mymost (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst)) (snd wins) (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf snd  wins
                    wins obj
                    max  score))))
        (values wins snd))))