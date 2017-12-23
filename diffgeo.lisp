;;;;
;;;; Differential Geometry
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defclass real-func ()
  ((sfunc :initarg :sfunc
	  :accessor sfunc
	  :type list
	  :documentation "The symbolic real function.")
   (func :initarg :func
	 :accessor func
	 :type function
	 :documentation "The funcallable real function.")
   (domain :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the function.")))
