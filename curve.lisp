;;;;
;;;; Curve objects.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defclass bezier-curve ()
  ((points :initarg :points
	   :initform #()
	   :accessor points)
   (degree :initarg :degree
	   :accessor degree))))
