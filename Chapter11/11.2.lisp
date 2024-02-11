;; ANSI Common Lisp: exercise 11.2


(defparameter *world* nil)

(defclass surface () 
    ((color :accessor color :initform nil :initarg :color)))

(defclass point ()
    ((x :accessor point-x :initform nil :initarg :x)
    (y :accessor point-y :initform nil :initarg :y)
    (z :accessor point-z :initform nil :initarg :z)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius :initform nil :initarg :radius)
    (center :accessor sphere-center :initform nil :initarg :center)))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere 
             :radius r
             :center (make-instance 'point :x x :y y :z z)
             :color  c)))
    (push s *world*)
    s))


;; I haven't tested the two methods below - should be fine, though. 
;; The LISP compiler doesn't complain :).

(defmethod intersect ((s sphere) (pt point) xr yr zr)
  (sphere-intersect s pt xr yr zr))

(defmethod normal ((s sphere) (pt point))
  (sphere-normal s pt))


;; PG's code
#|
(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x  (+ (x pt) (* n xr))
                    :y  (+ (y pt) (* n yr))
                    :z  (+ (z pt) (* n zr))))))


(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))
|#