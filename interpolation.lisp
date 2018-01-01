;;;;
;;;; Interpolation wrapper class around GSLL that stores interpolants.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter *interpolation-types*
  (list
   (cons 'linear           +linear-interpolation+)
   (cons 'polynomial       +polynomial-interpolation+)
   (cons 'cspline          +cubic-spline-interpolation+)
   (cons 'periodic-cspline +periodic-cubic-spline-interpolation+)
   (cons 'akima            +akima-interpolation+)
   (cons 'periodic-akima   +periodic-akima-interpolation+)))

(defparameter *interpolation-classes*
  (list
   (cons 'linear           0)
   (cons 'polynomial       +infinity-class+)
   (cons 'cspline          2)
   (cons 'periodic-cspline 2)
   (cons 'akima            2)
   (cons 'periodic-akima   2)))

(defclass rinterpolation ()
  ((xa :initarg :xa
       :accessor xa
       :type vector-double-float
       :documentation "The knots of the interpolation.")
   (ya :initarg :ya
       :accessor ya
       :type vector-double-float
       :documentation "The interpolants of the interpolation.")
   (interpolation :initarg interpolation
		  :accessor interpolation
		  :type interpolation
		  :documentation "The underlying GSLL interpolation object.")
   (itype :initarg :itype
	  :accessor itype
	  :type symbol
	  :documentation "The type of interpolation (per GSLL).")
   (size :initarg :size
	 :accessor size
	 :type integer)
   (c :initarg :c
      :accessor c
      :type (or integer symbol)
      :documentation "The differentiability class of the function.")
   (domain :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the function.")))

(defmethod print-object ((r rinterpolation) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "of class ~a over ~a with ~a ~(~a~) interpolants"
	    (%class-string% (c r)) (interval-string (domain r))
	    (size r) (itype r))))

(defun reinterpolate (rinterpolation)
  (with-slots (xa ya interpolation itype) rinterpolation
    (setf interpolation (make-interpolation
			 (cdr (assoc itype *interpolation-types*))
			 xa ya))))

(defmethod initialize-instance :after ((r rinterpolation) &key)
  (with-slots (xa ya interpolation itype size c domain) r
    (reinterpolate r)
    (setf size (dim0 xa))
    (setf c (cdr (assoc itype *interpolation-classes*)))
    (setf domain (interval (grid:aref xa 0) (grid:aref xa (1- size))))))

(defun make-rinterpolation (itype xa ya)
  (make-instance 'rinterpolation
		 :xa xa
		 :ya ya
		 :itype itype))

(defun make-uniform-knots (n &optional (a 0d0) (b 1d0))
  (make-grid-sequential-elements
   :dimensions n
   :grid-type 'foreign-array
   :element-type 'double-float
   :offset a
   :step-col (/ (- b a) (1- n))))

(defgeneric interpolate (object type &key)
  (:documentation "Interpolates object with interpolation type type."))

(defmethod interpolate ((object rlambda) type &key (n 128) (over (domain object)))
  (let* ((a (a over))
	 (b (b over))
	 (xa (make-uniform-knots n a b))
	 (ya (map-grid :source xa :element-function object)))
    (make-rinterpolation type xa ya)))

(defmethod interpolate ((object array) type &key)
  (let* ((ya (if (eq 'vector-double-float (type-of object))
		 object
		 (copy object :grid-type 'foreign-array)))
	 (size (dim0 ya))
	 (xa (make-uniform-knots size)))
    (make-rinterpolation type xa ya)))

(defmethod evaluate ((object rinterpolation) point &key)
  (evaluate (interpolation object) point
	    :xa (xa object) :ya (ya object)))

(defmethod evaluate-derivative ((object rinterpolation) point &key)
  (evaluate-derivative (interpolation object) point
		       :xa (xa object) :ya (ya object)))

(defmethod evaluate-second-derivative ((object rinterpolation) point &key)
  (evaluate-second-derivative (interpolation object) point
			      :xa (xa object) :ya (ya object)))

(defmethod evaluate-derivative* ((object rinterpolation) point &key)
  (values
   (evaluate object point)
   (evaluate-derivative object point)))

(defmethod evaluate-second-derivative* ((object rinterpolation) point &key)
  (values
   (evaluate object point)
   (evaluate-derivative object point)
   (evaluate-second-derivative object point)))

(defmethod evaluate-integral ((object rinterpolation) lower-limit upper-limit &key)
  (evaluate-integral (interpolation object) lower-limit upper-limit
		     :xa (xa object) :ya (ya object)))
