;;;;
;;;; The kitchen sink for curves, splines, screens, SDL stuff, etc.
;;;; Better organization sometime later.
;;;;
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defconstant tau (* 2d0 pi))
(declaim (type double-float tau))

(defparameter +arc-abs-error+ (* gsll:*default-absolute-error* 1.d2))
(defparameter +arc-rel-error+ 1d-3)

(defclass screen ()
  ((dims :initarg :dims
	 :accessor dims
	 :type 'vector-double-float
	 :documentation "The dimensions of the screen."))
  (:documentation "A screen that can be drawn upon."))

(defclass sdl-screen ()
  ((width :initarg :width
	  :accessor width
	  :type 'double-float
	  :documentation "The width of the screen.")
   (height :initarg :height
	   :accessor height
	   :type 'double-float
	   :documentation "The height of the screen."))
  (:documentation "A SDL screen that is drawn upon with SDL methods."))

(defgeneric screen-pos (p s &key)
  (:documentation "Maps the native coordinates of p to screen coordinates on s."))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key)
  "Lerps p in [0, 1] to screen size, preserving aspect ratio."
  (with-slots (width height) s
    (declare (type double-float width height))
    (let ((longdim (min width height)))
      (declare (type double-float longdim))
      (lerp (/ (+ p 1d0) 2d0) 0d0 longdim))))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key min max (margin t))
  "Lerps p in [min, max] to screen size, preserving aspect ratio and default margin."
  (with-slots (width height) s
    (declare (type double-float width height min max))
    (let ((longdim (min width height))
	  (factor (/ (sqrt 3d0))))
      (declare (type double-float longdim))
      (+ (if margin (* 0.5d0 (- 1d0 factor) longdim) 0d0)
	 (* (if margin factor 1d0)
	    (lerp (/ (- p min) (abs (- max min)))
		  0d0
		  longdim))))))

(defvar *cparam* 0d0
  "Color parameter used for rainbow output on draws.")
(declaim (type double-float *cparam*))

(defun multi-lerp (v &rest interpolants)
  "Piecewise lerps v between each of the interpolants. E.g. for interpolants
   (10 -10 10), the graph on v would follow a period of a triangle wave."
  (declare (type double-float v))
  (let ((len (the fixnum (length interpolants))))
    (when (< len 2)
      (error "multi-lerp needs two or more interpolants, but got ~a." len))
    (multiple-value-bind (k u) (floor (* v (1- len)))
      (let ((c (nthcdr k interpolants)))
	(lerp u (float (car c) 0d0) (float (cadr c) 0d0))))))

(defun draw-out-circle (x y &optional (hr 0.5d0) (filled nil) (color nil))
  "Draws a SDL circle at (x, y) with radius hr, with coloring."
  (let ((px (round x))
	(py (round y))
	(r (round (* hr 2)))
	(color (or color
		   (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
			      :g (floor (multi-lerp *cparam* 0   0   255 0))
			      :b (floor (multi-lerp *cparam* 0   255 0   0))))))
    (if filled
	(sdl:draw-filled-circle-* px py r :color color)
	(sdl:draw-aa-circle-* px py r :color color))))

(defun draw-out-line (x1 y1 x2 y2 &optional (color nil))
  "Draws a SDL line at (x1, y1) to (x2, y2), with coloring."
  (sdl-gfx:draw-line-*
   (round x1) (round y1) (round x2) (round y2)
   :color (or color
	      (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
			 :g (floor (multi-lerp *cparam* 0   0   255 0))
			 :b (floor (multi-lerp *cparam* 0   255 0   0))))))

(defgeneric draw (p s &key)
  (:documentation "Draws the object p on the screen s."))

(defmethod draw :before (p (s sdl-screen) &key (clear nil))
  "Handles clearing the screen before a draw when needed."
  (when clear
    (sdl:draw-rectangle-* 0 0
			  (floor (width s))
			  (floor (height s))
			  :color (sdl:color)
			  :alpha 16)))

(defmethod draw ((p plane-curve) (s sdl-screen) &key (n (size p)) (base nil) (normals nil))
  "Draws the plane-curve p on s with n lines, optionally drawing knots if :base t,
   and normals at the n line intersections if :normals t."
  (with-slots (width height) s
    (with-slots (x y domain) p
      (let* ((xs (make-array n :element-type 'double-float))
	     (ys (make-array n :element-type 'double-float))
	     (ns (make-array n :element-type '(simple-vector 2))))
	(loop :for i :from 0 :below n :do
	   (let* ((u (/ i (- n 1d0)))
		  (a (a domain))
		  (b (b domain))
		  (param (lerp u a b)))
	     (setf (aref xs i) (the double-float (evaluate x param))
		   (aref ys i) (the double-float (evaluate y param))
		   (aref ns i) (normalize (normal p param)))))
	(let* ((xmin (the double-float (loop :for i :from 0 :below n
					  :minimizing (aref xs i))))
	       (xmax (the double-float (loop :for i :from 0 :below n
					  :maximizing (aref xs i))))
	       (ymin (the double-float (loop :for i :from 0 :below n
					  :minimizing (aref ys i))))
	       (ymax (the double-float (loop :for i :from 0 :below n
					  :maximizing (aref ys i))))
	       (pmin (min xmin ymin))
	       (pmax (max xmax ymax))
	       (xnew nil)
	       (ynew nil)
	       (xold (screen-pos (aref xs 0) s :min pmin :max pmax))
	       (yold (screen-pos (aref ys 0) s :min pmin :max pmax)))
	  (loop :for i :from 1 :below n :do
	     (let* ((normal (aref ns i))
		    (xnorm (elt normal 0))
		    (ynorm (elt normal 1))
		    (nlen (* 256 (curvature p (/ i (- n 1d0))))))
	       (setf xnew (screen-pos (aref xs i) s :min pmin :max pmax)
		     ynew (- height (screen-pos (aref ys i) s :min pmin :max pmax)))
	       (draw-out-line xold yold xnew ynew)
	       (when normals
		 (draw-out-line xnew ynew (+ xnew (* nlen xnorm)) (+ ynew (* -1 nlen ynorm))))
	       (setf xold xnew yold ynew)))
	  (when base
	    (let* ((xis (xa x))
		   (yis (xa y))
		   (xod (ya x))
		   (yod (ya y))
		   (nx (the fixnum (size xis)))
		   (ny (the fixnum (size yis))))
	      (if (not (= nx ny))
		  (error "Components of ~a have different npoints: ~a and ~a"
			 p nx ny)
		  (loop :for i :from 0 :below (min nx ny) :do
		     (draw-out-circle (screen-pos (grid:aref xod i) s :min pmin :max pmax)
				      (- height
					 (screen-pos (grid:aref yod i) s :min pmin :max pmax))
				      1))))))))))

(defun draw-2d-particle (p s)
  "Draws a 2D particle to sdl-screen s."
  (with-slots (pos) p
    (with-slots (width height) s
      (let* ((px (aref pos 0))
	     (py (aref pos 1))
	     (x (+ px (/ width 2)))
	     (y (+ (- py) (/ height 2))))
	(draw-out-circle x y 1 t)))))

(defun draw-3d-particle (p s)
  "Draws a 3D particle to sdl-screen s with derpy weak-perspective projection."
  ;; TODO: Not necessarily here, but eventually implement actual 3D
  ;;       perspective projection.
  (with-slots (pos) p
    (with-slots (width height) s
      (let* ((px (aref pos 0))
	     (py (aref pos 1))
	     (pz (aref pos 2))
	     (k (sqrt 3))
	     (s (/ (min width height) 2d0))
	     (zscale (+ (* (/ (- k 1) (* -2 s)) (- pz s)) 1d0))
	     (x (+ (/ px zscale) (/ width 2)))
	     (y (+ (- (/ py zscale)) (/ height 2)))
	     (size-scale (/ zscale))
	     (color (round (* 255 size-scale)))
	     (hr (* 2d0 size-scale)))
	(draw-out-circle x y hr t (sdl:color :r color :g (round color 3) :b color))))))

(defmethod draw ((p particle) (s sdl-screen) &key)
  "Draws the particle p to sdl-screen s as a circle."
  (with-slots (pos) p
    (let ((dim (length pos)))
      (case dim
	(2 (draw-2d-particle p s))
	(3 (draw-3d-particle p s))
	(t (error "Cannot draw particle of dimensionality ~d to sdl-screen." dim))))))

(defmethod draw :after ((p generator-bullet) (s sdl-screen) &key)
  (with-slots (children) p
    (loop :for i :from 2 :to (queue-count children) :do
       (draw (svref children i) s))))
