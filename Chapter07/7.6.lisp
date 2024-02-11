;; ANSI Common Lisp: exercise 7.6

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
        (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))


;; old is a list of characters and wildcards
;; characters are matched as they are
;; wildcards: d = digits; a = alphanumeric; + = any character
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof) (or (setf from-buf (buf-next buf)) (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((match-char old c pos)             ; only line changed
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))


;; matches the character c with the element at position pos in the pattern old
;; old is a list of characters and wildcards
;; characters are matched as they are
;; wildcards: d = digits; a = alphanumeric; + = any character
(defun match-char (old c pos)
  (let ((e (nth pos old)))
    (if (typep e 'character)
      (char= c e)
      (cond
        ((eql e 'd) (digit-char-p c))
        ((eql e 'a) (alphanumericp c))
        ((eql e '+) t)
        (t nil)))))


;; uses string streams
(defun string-subst (old new str)
  (with-input-from-string (sstr1 str)
    (with-output-to-string (sstr2)
      (stream-subst old new sstr1 sstr2))))


;; pattern must be a list of characters and/or wildcards
;; wildcards: d = digits; a = alphanumeric; + = any character
(string-subst '(#\p #\p a) "WWW" "pappa gorgia")
(string-subst '(#\p #\p d) "WWW" "pappa gorgia")
(string-subst '(#\z #\a) "WW" "pappa gorgia")
