;; ANSI Common Lisp: exercise 6.5

(defun myremove-if (pred lst)
"returns a list in which all the elements of lst satisfying pred are removed"
    (filter (complement pred) lst))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push x acc))))             ; mod: pushes obj instead of val
    (nreverse acc)))

;; testing
(myremove-if #'characterp '(#\a #\b 1 3 "ciao" poi))
(myremove-if #'stringp '(#\a #\b 1 3 "ciao" poi))