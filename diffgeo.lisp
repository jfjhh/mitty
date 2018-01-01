;;;;
;;;; Differential Geometry
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(deftype rfunc () '(or rinterpolation))

(defclass curve ()
  ((domain :initform (interval 0d0 1d0)
	   :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the curve.")
   (size :initarg :size
	 :accessor size
	 :type integer
	 :documentation "The number of knots in the curve.")
   (c :initarg :c
      :accessor c
      :type (or integer symbol)
      :documentation "The differentiability class of the curve.")
   (knots :initarg :knots
	  :accessor knots
	  :type vector-double-float
	  :documentation "The knots of the curve.")))

(defmethod print-object ((c curve) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "of class ~a over ~a with ~a interpolants"
	    (%class-string% (c c)) (interval-string (domain c)) (size c))))

(defgeneric curve-point (curve u)
  (:documentation "Gets the point on the curve at u."))

(defgeneric tangent (curve u)
  (:documentation "Gets the tangent vector to curve at u."))

(defgeneric normal (curve u)
  (:documentation "Gets the normal vector to curve at u."))

(defgeneric curvature (curve u)
  (:documentation "Gets the signed curvature of curve at u."))

(defgeneric curve-map (function curve &key)
  (:documentation "Transforms the curve with function."))

(defgeneric local-map (function curve param i &key)
  (:documentation "Transforms the curve at param with (function param curve)."))

(defmethod curve-map (function (curve curve) &rest args &key)
  (with-slots (size knots) curve
    (dotimes (i size)
      (let ((param (grid:aref knots i)))
	(apply #'local-map function curve param i args)))))

(defclass plane-curve (curve)
  ((x :initarg :x
      :accessor x
      :type rfunc
      :documentation "The abscissa of the plane curve.")
   (y :initarg :y
      :accessor y
      :type rfunc
      :documentation "The ordinate of the plane curve.")))

(defun make-plane-curve (x y &optional knots)
  (let ((xs (size x))
	(ys (size y))
	(domain (intersect (domain x) (domain y))))
    (when (/= xs ys)
      (error "Size mismatch between x and y in make-plane-curve:~%~a~%~a" x y))
    (make-instance 'plane-curve
		   :x x
		   :y y
		   :domain domain
		   :size xs
		   :c (min (c x) (c y))
		   :knots (or knots (make-uniform-knots xs (a domain) (b domain))))))

(defmethod curve-point ((curve plane-curve) u)
  (grid (evaluate (x curve) u)
	(evaluate (y curve) u)))

(defmethod tangent ((curve plane-curve) u)
  (grid (evaluate-derivative (x curve) u)
	(evaluate-derivative (y curve) u)))

(defmethod normal ((curve plane-curve) u)
  (grid (- (evaluate-derivative (y curve) u))
	(evaluate-derivative (x curve) u)))

(defmethod curvature ((curve plane-curve) u)
  (multiple-value-bind (x xd xdd) (evaluate-second-derivative* (x curve) u)
    (declare (ignore x))
    (multiple-value-bind (y yd ydd) (evaluate-second-derivative* (y curve) u)
      (declare (ignore y))
      (/ (- (* xd ydd) (* yd xdd)) (expt (+ (* xd xd) (* yd yd)) 3/2)))))

(defmethod curve-map :after (function (curve plane-curve) &key (impure nil))
  (when impure
    (reinterpolate (x curve))
    (reinterpolate (y curve))))

(defmethod local-map (function (curve plane-curve) param i &key (impure nil))
  (let ((point (funcall function curve param i)))
    (when impure
      (setf (grid:aref (ya (x curve)) i) (grid:aref point 0))
      (setf (grid:aref (ya (y curve)) i) (grid:aref point 1)))))

(defun displace (curve function)
  (flet ((disp (c u i)
	   (with-slots (x y) c
	     (let ((current (grid (grid:aref (ya x) i)
				  (grid:aref (ya y) i))))
	       (antik:+ current (funcall function c u i))))))
    (curve-map #'disp curve :impure t)))

(defun minimize-curvature (curve u i)
  (declare (ignore i))
  (let* ((n (normalize (normal curve u)))
	 (k (curvature curve u))
	 (sgn (if (plusp k) 1 -1))
	 (s (* sgn (clamp (* (abs k) 0.005d0) 0d0 0.1d0))))
    (antik:* n s)))

(defun jiggle (amount)
  (let ((amount (clamp amount 0d0 0.5d0)))
    (flet ((disp () (- (random 1.0d0) 0.5d0)))
      (lambda (&rest args)
	(declare (ignore args))
	(antik:* (normalize (grid (disp) (disp)))
		 amount)))))

(defun draw-deform (curve function &optional (n 1))
  (dotimes (i n)
    (sdl:draw-rectangle-* 0 0
			  (floor (width *screen*))
			  (floor (height *screen*))
			  :color (sdl:color)
			  :alpha 32)
    (displace curve function)
    (draw curve *screen* :n (* 3 (size curve)) :clear nil :base t :normals t)))
