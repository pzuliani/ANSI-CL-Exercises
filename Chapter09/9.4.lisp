;; ANSI Common Lisp: exercise 9.4

;; The main idea is to extend the segments to lines and then check whether the lines are 
;; parallel or not. Another approach entails taking convex combinations of the two points
;; and solve a linear programming problem.


(defstruct (line (:conc-name nil))         ; the line y = cx + b 
    (c nil) (b 0.0))    ; if c = nil then it represents the line x = b

(defstruct (point (:conc-name nil)) 
    x y)   ; a point in 2D space


;; computes the line passing by points a and b
;; assumes a and b different
(defun compute-line (a b)   
    (let ((l (make-line)) (tmp (- (x a) (x b))))
        (if (= tmp 0.0)
            (setf (b l) (x a))       ; segment (a,b) is parallel to the y axis
            (let ((c (/ (- (y a) (y b)) tmp)))      ; compute slope
                (setf (c l) c 
                    (b l) (- (y a) (* c (x a))))))
    l))


;; evaluates a line at input x
;; returns nil if line is parallel to y axis
(defun eval-line (l x)
    (if (null (c l))
        nil                 ; line is parallel to y axis
        (+ (b l) (* (c l) x))))


;; returns an intersection point between the intervals [a,b] and [c,d], where a <= b and c <= d
;; returns nil if the intervals are disjoint
(defun intersect-intervals (a b c d)
    (if (or (< b c) (< d a))
        nil
        (/ (+ b c) 2)))


;; returns true if point p belongs to interval [a, b] (a<=b), nil otherwise
(defun point-in-interval (p a b)
    (not (or (< b p) (< p a))))


;; compute an intersection point between the 2D segments (a,b) and (c,d)
;; returns nil if the segments do not intersect
(defun intersect-segments (a b c d)
    (let ((line1 (compute-line a b)) (line2 (compute-line c d)))
        (if (eql (c line1) (c line2))           ; are the segments parallel?
            (if (= (b line1) (b line2))       ; are the segments on the same line?
                (if (null (c line1))               ; are the segments parallel to y axis?
                    ; segments are on the same line and parallel to y axis
                    (let ((yi (intersect-intervals (min (y a) (y b)) (max (y a) (y b))  
                            (min (y c) (y d)) (max (y c) (y d)))))
                        (if (null yi)
                            nil
                            (make-point :x (b line1) :y yi)))
                    ; segments are on the same line but NOT parallel to y axis
                    (let ((xi (intersect-intervals (min (x a) (x b)) (max (x a) (x b)) 
                            (min (x c) (x d)) (max (x c) (x d))))
                        (yi (intersect-intervals (min (eval-line line1 (y a)) (eval-line line1 (y b))) 
                            (max (eval-line line1 (y a)) (eval-line line1 (y b)))  
                            (min (eval-line line2 (y c)) (eval-line line2 (y d))) 
                            (max (eval-line line2 (y c)) (eval-line line2 (y d))))))
                        (if (and (null xi) (null yi))
                            nil
                            (make-point :x xi :y yi))))
                nil)            ; segments are parallel but NOT on the same line
            (progn       ; segments are not parallel: compute intersection of the lines
                (cond
                    ((null (c line1))          ; is segment (a,b) parallel to y axis?
                        (setf xi (b line1)
                            yi (eval-line line2 xi))) 
                    ((null (c line2))          ; is segment (c,d) parallel to y axis?
                        (setf xi (b line2)
                            yi (eval-line line1 xi))) 
                    (t (setf xi (/ (- (b line2) (b line1)) (- (c line1) (c line2)))
                            yi (eval-line line1 xi))))
                ;; checks whether yi belongs to both segments
                (if (and (point-in-interval yi (min (y a) (y b)) (max (y a) (y b)))
                        (point-in-interval yi (min (y c) (y d)) (max (y c) (y d))))
                    (make-point :x xi :y yi)
                    nil)))))


;; some tests
(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 2.0 :y 2.0) (make-point :x 8.0 :y 8.0))      
;#S(POINT :X 2.0 :Y 2.0)    OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 1.5 :y 1.5) (make-point :x 8.0 :y 8.0)) 
;#S(POINT :X 1.75 :Y 1.75)  OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 1.0 :y 2.0)
    (make-point :x 1.0 :y 2.0) (make-point :x 1.0 :y 8.0)) 
;#S(POINT :X 1.0 :Y 2.0)    OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 1.0 :y 2.0)
    (make-point :x 1.0 :y 1.5) (make-point :x 1.0 :y 8.0))
;#S(POINT :X 1.0 :Y 1.75)   OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 1.0 :y 2.0)
    (make-point :x 1.0 :y 2.5) (make-point :x 1.0 :y 8.0))
;NIL        OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 4.0 :y 1.0) (make-point :x 5.0 :y 2.0))
;NIL        OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 3.0 :y 3.0) (make-point :x 5.0 :y 5.0))
;NIL        OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 2.0 :y 1.0) (make-point :x 1.0 :y 2.0))
;#S(POINT :X 1.5 :Y 1.5)        OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 3.0 :y 1.8) (make-point :x 2.0 :y 3.0))
;NIL        OK

(intersect-segments (make-point :x 1.0 :y 1.0) (make-point :x 2.0 :y 2.0)
    (make-point :x 1.0 :y 2.0) (make-point :x 2.0 :y 2.0))
;#S(POINT :X 2.0 :Y 2.0)        OK