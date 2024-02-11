
;; ANSI Common Lisp: exercise 13.3a

(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)
(declaim (fixnum yzero))

(defun leap? (y)
  (declare (fixnum y))
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (declare (fixnum d m y))
  (with-type fixnum (+ (- d 1) (month-num m y) (year-num y))))

(defun month-num (m y)
  (declare (fixnum m y))
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (declare (fixnum y))
  (let ((d 0))
    (declare (fixnum d))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (with-type fixnum (incf d (year-days (+ yzero i)))))
        (dotimes (i (- yzero y) (- d))
          (with-type fixnum (incf d (year-days (+ y i))))))))

(defun year-days (y) 
  (declare (fixnum y))
  (if (leap? y) 366 365))

(defun num->date (n)
  (declare (fixnum n))
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (declare (fixnum n))
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (declare (fixnum n y))
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t        (nmon n)))
      (nmon n)))

(defun nmon (n)
  (declare (fixnum n))
  (let ((m (position n month :test #'<)))
    (declare (fixnum m))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun date+ (d m y n)
  (declare (fixnum d m y n))
  (num->date (+ (date->num d m y) n)))


;; More Paul Graham's code (from his book's notes)
(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr) 
                   expr
                   (expand-call type (binarize expr)))))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a) 
                              `(with-type ,type ,a))
                          (cdr expr))))

(defun binarize (expr)
  (if (and (nthcdr 3 expr)
           (member (car expr) '(+ - * /)))
      (destructuring-bind (op a1 a2 . rest) expr
        (binarize `(,op (,op ,a1 ,a2) ,@rest)))
      expr))
