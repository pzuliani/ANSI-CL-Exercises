;; ANSI Common Lisp: exercise 11.1

(defclass rectangle ()
    ((height :accessor rectangle-height :initform 0 :initarg :height)
    (width :accessor rectangle-width :initform 0 :initarg :width))) 

(defclass circle ()
    ((radius :accessor circle-radius :initform 0 :initarg :radius)))

(defmethod area ((x rectangle))
    (* (rectangle-height x) (rectangle-width x)))

(defmethod area ((x circle))
    (* pi (expt 2 (circle-radius x))))

(let ((r (make-instance 'rectangle :height 2 :width 3))
        (c (make-instance 'circle :radius 1)))
    (format t "Rectangle area = ~A~%Circle area = ~A~%" (area r) (area c)))