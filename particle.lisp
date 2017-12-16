;;;;
;;;; Particle motion.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defclass particle ()
  ((pos :initform (grid:make-foreign-array
		   'double-float
		   :initial-contents '(0d0 0d0))
	:initarg :pos
	:accessor pos
	:type 'grid:vector-double-float)))

(defclass generator ()
  ((ndims :initform 2
	  :initarg :ndims
	  :accessor ndims
	  :type 'integer)
   (npoints :initarg :npoints
	    :accessor npoints
	    :type 'integer)
   (dims :accessor dims
	 :type 'simple-vector)))

(defclass splinerator (generator)
  ((nevals :initarg :nevals
	   :accessor nevals
	   :type 'integer)
   (gsplines :accessor gsplines
	   :type 'simple-vector)
   (sdims :accessor sdims
	  :type 'simple-vector)
   (knots :accessor knots
	  :type 'grid:vector-double-float)))

(defmethod initialize-instance :after ((g generator) &key)
  (with-slots (ndims npoints dims) g
    (setf dims (make-array ndims))
    (loop :for i :below ndims :do
       (setf (aref dims i)
	     (grid:make-foreign-array 'double-float
				      :dimensions npoints)))))

(defmethod initialize-instance :after ((s splinerator) &key)
  (with-slots (npoints nevals sdims ndims gsplines knots) s
    (setf sdims (make-array ndims))
    (setf gsplines (make-array ndims))
    (setf knots (grid:make-foreign-array 'double-float :dimensions npoints))
    (loop :for i :below ndims :do
       (setf (aref sdims i)
	     (grid:make-foreign-array 'double-float
				      :dimensions nevals)))))

(defclass rot-particle (particle)
  ((vel :initform 0d0
	:initarg :vel
	:accessor vel
	:type 'double-float)
   (theta :initform 0d0
	  :initarg :theta
	  :accessor theta
	  :type 'double-float)
   (omega :initform 0d0
	  :initarg :omega
	  :accessor omega
	  :type 'double-float)))

(defclass spell ()
  ((parts :initform (make-array 0 :fill-pointer 0 :element-type '(or particle spell))
	  :initarg :parts
	  :accessor parts
	  :type 'simple-vector)
   (nups :initform 0
	 :accessor nups
	 :type 'integer)
   (genf :initform (constantly nil)
	 :initarg :genf
	 :accessor genf)))

(defgeneric generate (g dimfuncs &key start end)
  (:documentation "Generates g with the vector results of dimfuncs on [0,1]."))

(defgeneric draw (p s &key)
  (:documentation "Draws the particle p on the screen s."))

(defgeneric particle-collide (p)
  (:documentation "Collides the particle with the boundaries."))

(defgeneric update (p dt)
  (:documentation "Updates the moving object p with time step dt."))

(defmethod generate ((g generator) dimfuncs &key (start 0d0) (end 1d0))
  (with-slots (ndims npoints dims) g
    (loop :for i :from 0 :below npoints :do
       (let ((u (float (lerp (/ i (1- npoints)) start end) 0d0)))
	 (loop :for j :from 0 :below ndims :do
	    (setf (grid:aref (aref dims j) i)
		  (funcall (aref dimfuncs j) u)))))))

(defmethod generate :after ((s splinerator) dimfuncs &key (start 0d0) (end 1d0))
  (declare (ignore dimfuncs))
  (with-slots (dims sdims ndims npoints nevals gsplines knots) s
    (loop :for i :from 0 :below npoints :do
       (setf (grid:aref knots i)
	     (float (lerp (/ i (1- npoints)) start end) 0d0)))
    (setf gsplines
	  (map
	   'vector
	   (curry #'gsll:make-spline
		  gsll:+cubic-spline-interpolation+
		  knots)
	   dims))
    (loop :for i :from 0 :below ndims :do
       (loop :for j :from 0 :below nevals :do
	  (setf (grid:aref (aref sdims i) j)
		(gsll:evaluate
		 (aref gsplines i)
		 (float (lerp (/ j (1- nevals)) start end) 0d0)))))))

(defparameter *cparam* 0d0)

(defun multi-lerp (v &rest interpolants)
  (let ((len (length interpolants)))
    (multiple-value-bind (k u) (floor (* v (1- len)))
      (let ((c (nthcdr k interpolants)))
	(lerp u (car c) (cadr c))))))

(defmethod draw ((p particle) (s sdl-screen) &key)
  (with-slots (width height) s
    (let* ((coords (screen-pos p s))
	   (px (car coords))
	   (py (cadr coords)))
      (setf *cparam* (mod (+ *cparam* 0.005d0) 1d0))
      (draw-out-circle px py 1))))

(defmethod draw ((g generator) (s sdl-screen) &key (hr 5) (pmin nil) (pmax nil))
  (with-slots (width height) s
    (with-slots (dims ndims npoints) g
      (let* ((coords
	      (map
	       'vector
	       (lambda (d)
		 (let ((pmin (or pmin (loop :for i :from 0 :below npoints
					 :minimizing (grid:aref d i))))
		       (pmax (or pmax (loop :for i :from 0 :below npoints
					 :maximizing (grid:aref d i)))))
		   (grid:map-grid :source d
				  :element-function
				  (lambda (x) (screen-pos x s :min pmin :max pmax)))))
	       dims)))
	(setf *cparam* (mod (+ *cparam* 0.005d0) 1d0))
	(loop :for i :from 0 :below npoints :do
	   (let ((cs (loop :for j :from 0 :below 2
			:collecting
			(let ((coord (grid:aref (aref coords j) i)))
			  (if (= j 1) (- height coord) coord)))))
	     (draw-out-circle (car cs) (cadr cs) hr)))))))

(defmethod draw ((p splinerator) (s sdl-screen) &key (hr 1/2) (base nil) (clear nil))
  (declare (ignore hr))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (sdims ndims nevals) p
      (let* ((pmin nil)
	     (pmax nil)
	     (coords
	      (map
	       'vector
	       (lambda (d)
		 (setf pmin (loop :for i :from 0 :below nevals
			       :minimizing (grid:aref d i)))
		 (setf pmax (loop :for i :from 0 :below nevals
			       :maximizing (grid:aref d i)))
		 (grid:map-grid :source d
				:element-function
				(lambda (x) (screen-pos x s :min pmin :max pmax))))
	       sdims))
	     (oldcs (loop :for j :from 0 :below 2
		       :collecting
		       (let ((coord (grid:aref (aref coords j) 0)))
			 (if (= j 1) (- height coord) coord)))))
	(loop :for i :from 1 :below nevals :do
	   (let ((newcs
		  (loop :for j :from 0 :below 2
		     :collecting
		     (let ((coord (grid:aref (aref coords j) i)))
		       (if (= j 1) (- height coord) coord)))))
					;(draw-out-circle (car oldcs) (cadr oldcs) hr)
	     (draw-out-line (car oldcs) (cadr oldcs)
			    (car newcs) (cadr newcs))
	     (setf oldcs newcs)))
	(when base
	  (call-next-method p s :hr 2
			    ;:pmin pmin :pmax pmax
			    ))))))

(defmethod draw ((l spell) (s sdl-screen) &key (clear t))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (parts) l
      (loop :for p :being :the :elements :of parts :do
	 (draw p s)))))

(defun draw-out-circle (x y &optional (hr 1/2))
  (setf *cparam* (mod (+ *cparam* 0.005d0) 1d0))
  (sdl:draw-filled-circle
   (sdl:point :x x :y y)
   (* hr 2)
   :color (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
		     :g (floor (multi-lerp *cparam* 0   0   255 0))
		     :b (floor (multi-lerp *cparam* 0   255 0   0)))))

(defun draw-out-line (x1 y1 x2 y2)
  (setf *cparam* (mod (+ *cparam* 0.001d0) 1d0))
  (sdl-gfx:draw-line-*
   (floor x1)
   (floor y1)
   (floor x2)
   (floor y2)
   :color (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
		     :g (floor (multi-lerp *cparam* 0   0   255 0))
		     :b (floor (multi-lerp *cparam* 0   255 0   0)))))

(defmethod particle-collide ((p rot-particle))
  (with-slots (pos vel theta omega) p
    (let ((x (grid:aref pos 0))
	  (y (grid:aref pos 1)))
      ;; Wall wrapping.
      (when (< x -1d0)
	(setf (grid:aref pos 0)  1d0))
      (when (> x 1d0)
	(setf (grid:aref pos 0) -1d0))
      (when (< y -1d0)
	(setf (grid:aref pos 1)  1d0))
      (when (> y 1d0)
	(setf (grid:aref pos 1) -1d0))

      #||
      ;; Wall bouncing.
      (when (< x -1d0)
	(setf (grid:aref pos 0) -1d0)
	(setf theta (- pi theta)))
      (when (> x 1d0)
	(setf (grid:aref pos 0)  1d0)
	(setf theta (- pi theta)))
      (when (< y -1d0)
	(setf (grid:aref pos 1) -1d0)
	(setf theta (- tau theta)))
      (when (> y 1d0)
	(setf (grid:aref pos 1)  1d0)
	(setf theta (- tau theta)))
      ||#
      )))

(defmethod update ((p rot-particle) (dt number))
  (with-slots (pos vel theta omega) p
    (setf theta (mod (+ theta (* omega dt)) tau))
    (incf (grid:aref pos 0) (* vel (cos theta)))
    (incf (grid:aref pos 1) (* vel (sin theta)))
    (particle-collide p)))

(defmethod update ((s spell) (dt number))
  (with-slots (parts nups genf) s
    (incf nups)
    (let ((addition (funcall genf s dt)))
      (when addition
	(vector-push-extend addition parts)))
    (loop :for p :being :the :elements :of parts :do
       (update p dt))))
