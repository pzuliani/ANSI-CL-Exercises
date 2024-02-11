
;; ANSI Common Lisp: exercise 13.3b

(defun sq (x) 
    (declare (long-float x))
    (with-type long-float (* x x)))

(defun mag (x y z)
  (declare (long-float x y z))
  (with-type long-float (sqrt (+ (sq x) (sq y) (sq z)))))

(defun unit-vector (x y z)
  (declare (long-float x y z))
  (let ((d (mag x y z)))
    (declare (long-float d))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))  
  x y z)

(defun distance (p1 p2)
  (with-type long-float (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2)))))

(defun minroot (a b c)
  (declare (long-float a b c))
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (declare (long-float disc))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (declare (long-float discrt))
            (with-type long-float (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a)))))))))


(defstruct surface  color)

(defparameter *world* nil)

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (declare (long-float res))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (declare (long-float inc))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defun color-at (x y)
  (declare (long-float x y))
  (multiple-value-bind (xr yr zr) 
                       (unit-vector (- x (x eye))
                                    (- y (y eye))
                                    (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (declare (long-float xr yr zr))
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (declare (long-float d))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (declare (long-float xr yr zr))
  (multiple-value-bind (xn yn zn) (normal s int)
    (with-type long-float (max 0 (+ (* xr xn) (* yr yn) (* zr zn))))))


(defstruct (sphere (:include surface))  
  radius center)

(defun defsphere (x y z r c)
  (declare (long-float x y z r))
  (let ((s (make-sphere 
             :radius r
             :center (make-point :x x :y y :z z)
             :color  c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (declare (long-float xr yr zr))
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (declare (long-float xr yr zr))
  (let* ((c (sphere-center s))
         (n (with-type long-flot (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s))))))))
    (if n
        (make-point :x  (+ (x pt) (* n xr))
                    :y  (+ (y pt) (* n yr))
                    :z  (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))


(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))

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