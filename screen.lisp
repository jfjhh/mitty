;;;;
;;;; Screens for output.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defclass screen ()
  ((dims :initarg dims
	 :accessor dims
	 :type 'grid:vector-double-float)))

(defclass sdl-screen ()
  ((width :initarg :width
	  :accessor width
	  :type 'integer)
  (height :initarg :height
	  :accessor height
	  :type 'integer)))

(defgeneric screen-pos (p s &key)
  (:documentation "Maps the particle coordinates of p to screen coordinates on s."))

(defmethod screen-pos ((p particle) (s sdl-screen) &key)
  (with-slots (pos) p
    (with-slots (width height) s
      (let ((px (grid:aref pos 0))
	    (py (grid:aref pos 1)))
	(list (lerp (/ (+ px 1) 2) 0 width)
	      (lerp (- 1 (/ (+ py 1) 2)) 0 height))))))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key)
  (with-slots (width height) s
    (let ((longdim (min width height)))
      (lerp (/ (+ p 1) 2) 0 longdim))))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key min max)
  (with-slots (width height) s
    (let ((longdim (min width height)))
      (lerp (/ (- p min) (abs (- max min)))
	    0
	    longdim))))
