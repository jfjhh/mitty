;;;;
;;;; Differential Geometry
;;;; Alex Striff
;;;;

(in-package #:mitty)

(deftype rfunc () '(or rinterpolation))

(defclass curve ()
  ((domain :initarg :domain
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
	  :documentation "The knots of the curve."))
  (:default-initargs :domain (interval 0d0 1d0)))

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
  (:documentation "Transforms the curve globally by local-mapping function on curve."))

(defgeneric local-map (function curve param i &key)
  (:documentation "Transforms the curve point at param to be the result of
calling function with the curve, the value of the parameter along the curve,
and the index of the point."))

(defgeneric displace (curve function)
  (:documentation "Displaces the curve at its points by the vector returned by
function evaluated as per local-map."))

(defmethod curve-map (function (curve curve) &rest args &key)
  ;; General to all curves with underlying knot parameters.
  (with-slots (size knots) curve
    (dotimes (i size)
      (let ((param (aref knots i)))
	(apply #'local-map function curve param i args)))))

(defclass plane-curve (curve)
  ((x :initarg :x
      :accessor x
      :type rfunc
      :documentation "The abscissae of the plane curve.")
   (y :initarg :y
      :accessor y
      :type rfunc
      :documentation "The ordinates of the plane curve.")))

(defun make-plane-curve (x y &optional knots)
  "Makes a plane curve with abscissae x and ordinates y, with default uniform knots."
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
      (setf (aref (ya (x curve)) i) (aref point 0))
      (setf (aref (ya (y curve)) i) (aref point 1)))))

(defmethod displace (curve function)
  (flet ((disp (c u i)
	   (with-slots (x y) c
	     (let ((current (grid (aref (ya x) i)
				  (aref (ya y) i))))
	       (antik:+ current (funcall function c u i))))))
    (curve-map #'disp curve :impure t)))

(defun minimize-curvature (curve u i &optional (step-size 0.0025))
  "An attempt at a displace function that will minimize curvature of curve upon
   iteration. Currently requires sufficiently small step-size to be stable.
   TODO: Implement springiness so that points that are moved closer together will
         also be pushed apart."
  (declare (ignore i))
  (let* ((n (normalize (normal curve u)))
	 (k (curvature curve u))
	 (sgn (if (plusp k) 1 -1))
	 (s (* sgn (clamp (* (abs k) step-size) 0d0 0.1d0))))
    (antik:* n s)))

(defun jiggle (amount)
  "A displace function that generates vectors in the closed disk of radius amount."
  (let ((amount (clamp amount 0d0 0.5d0)))
    (flet ((disp () (- (random 1.0d0) 0.5d0)))
      (lambda (&rest args)
	(declare (ignore args))
	(antik:* (normalize (grid (disp) (disp)))
		 amount)))))

(defun draw-deform (curve function &optional (n 1))
  "Performs n displacements of curve by function, while drawing curve each iteration."
  (dotimes (i n)
    (sdl:draw-rectangle-* 0 0
			  (floor (width *screen*))
			  (floor (height *screen*))
			  :color (sdl:color)
			  :alpha 32)
    (displace curve function))
  (draw curve *screen* :n (* 3 (size curve)) :clear nil :base t :normals t))
