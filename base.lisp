;;;;
;;;; The kitchen sink for particles, splines, screens, etc.
;;;; Better organization sometime later.
;;;;
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defconstant tau (* 2 pi))

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

(defclass particle ()
  ((alive :initform t
	  :accessor alive
	  :type '(boolean null))
   (pos :initform (grid:make-foreign-array
		   'double-float
		   :initial-contents '(0d0 0d0))
	:initarg :pos
	:accessor pos
	:type 'grid:vector-double-float)))

(defclass data-points ()
  ((npoints :initarg :npoints
	    :accessor npoints
	    :type 'integer)
   (points :accessor points
	   :type 'grid:vector-double-float)
   (start :initarg :start
	  :accessor start
	  :type 'real)
   (end :initarg :end
	:accessor end
	:type 'real)))

(defclass parametric-points (data-points)
  ((func :initarg :func
	 :accessor func
	 :type 'function)))

(defclass interpolation-curve ()
  ((interpolants :initarg :interpolants
		 :accessor interpolants
		 :type 'data-points)))

(defclass spline-curve (interpolation-curve)
  ((spline :accessor spline
	   :type 'gsll:spline)
   (ducks :accessor ducks
	  :type 'grid:vector-double-float)))

(defclass plane-curve ()
  ((x :initarg :x
      :accessor x
      :type 'interpolation-curve)
   (y :initarg :y
      :accessor y
      :type 'interpolation-curve)))

(defclass rot-particle (particle)
  ((vel :initform 0d0
	:initarg :vel
	:accessor vel
	:type 'double-float)
   (acc :initform 0d0
	:initarg :acc
	:accessor acc
	:type 'double-float)
   (theta :initform 0d0
	  :initarg :theta
	  :accessor theta
	  :type 'double-float)
   (omega :initform 0d0
	  :initarg :omega
	  :accessor omega
	  :type 'double-float)))

(defclass spell (rot-particle)
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

(defmethod initialize-instance :after ((d data-points) &key)
  (with-slots (npoints points) d
    (setf points (grid:make-foreign-array 'double-float :dimensions npoints))))

(defmethod initialize-instance :after ((p parametric-points) &key)
  (with-slots (npoints points func start end) p
    (loop :for i :from 0 :below npoints :do
       (setf (grid:aref points i) (funcall func (lerp (/ i (1- npoints)) start end))))))

(defmethod initialize-instance :after ((s spline-curve) &key)
  (with-slots (spline ducks interpolants) s
    (with-slots (npoints points start end) interpolants
      (setf ducks (grid:make-foreign-array 'double-float :dimensions npoints))
      (loop :for i :from 0 :below npoints :do ;; Uniform spline ducks.
	 (setf (grid:aref ducks i) (float (lerp (/ i (1- npoints)) start end) 0d0)))
      (setf spline (gsll:make-spline gsll:+cubic-spline-interpolation+ ducks points)))))

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

(defmethod screen-pos ((p double-float) (s sdl-screen) &key min max (shrink t))
  (with-slots (width height) s
    (let ((longdim (min width height)))
      (+ (if shrink (* 1/2 1/8 longdim) 0)
	 (* (if shrink 7/8 1)
	    (lerp (/ (- p min) (abs (- max min)))
		  0
		  longdim))))))

(defgeneric draw (p s &key)
  (:documentation "Draws the particle p on the screen s."))

(defgeneric particle-collide (p)
  (:documentation "Collides the particle with the boundaries."))

(defgeneric update (p dt)
  (:documentation "Updates the moving object p with time step dt."))

(declaim (inline quadrance))
(defun quadrance (&rest nums)
  (declare (optimize (speed 3) (safety 1)))
  (reduce (lambda (x y)
	    (declare (type double-float x y))
	    (+ x (* y y))) nums :initial-value 0d0))
(declaim (notinline quadrance))

(declaim (inline norm))
(defun norm (&rest nums)
  (declare (optimize (speed 3) (safety 1))
	   (inline quadrance sqrt))
  (sqrt (the (double-float 0d0) (apply #'quadrance nums))))
(declaim (notinline norm))

(declaim (inline vec-slope))
(defun vec-slope (vec)
  (declare (optimize (speed 3) (safety 1))
	   (type (simple-vector 2) vec))
  (let ((x (aref vec 0))
	(y (aref vec 1)))
    (declare (type double-float x y))
    (atan y x)))
(declaim (notinline vec-slope))

(defgeneric param (p u)
  (:documentation "Lerps u in [0,1] to p's native domain."))

(defmethod param ((p interpolation-curve) (u real))
  (when (or (< u 0) (< 1 u))
    (error "param on ~a was called with ~a not in [0,1]." p u))
  (with-slots (start end) (interpolants p)
    (lerp u start end)))

(defgeneric arc-length (curve u &optional v)
  (:documentation "Calculates the arc length of curve from 0 to u,
or between u and v if v is supplied. u and v are in [0,1]."))

(defmethod arc-length ((object spline-curve) (u double-float) &optional v)
  (let ((a (if v u 0d0))
	(b (or v u))
	(ds (lambda (u) (sqrt (+ 1d0 (expt (evaluate-derivative object u) 2))))))
    (gsll:integration-qag ds a b :gauss61)))

(defmethod arc-length ((object plane-curve) (u double-float) &optional v)
  (with-slots (x y) object
    (let ((a (if v u 0d0))
	  (b (or v u))
	  (ds (lambda (u) (sqrt (+ (expt (evaluate-derivative x u) 2)
			      (expt (evaluate-derivative y u) 2))))))
      (gsll:integration-qag ds a b :gauss61))))

(defmethod evaluate ((object parametric-points) (u real) &key)
  (with-slots (func start end) object
    (funcall func (lerp u start end))))

(defmethod evaluate ((object spline-curve) (x real) &key)
  (gsll:evaluate (spline object) (float x 0d0)))

(defmethod evaluate-derivative ((object spline-curve) (x real) &key)
  (gsll:evaluate-derivative (spline object) (float x 0d0)))

(defmethod evaluate-second-derivative ((object spline-curve) (x real) &key)
  (gsll:evaluate-second-derivative (spline object) (float x 0d0)))

(defmethod evaluate-integral ((object spline-curve)
			      (lower-limit real)
			      (upper-limit real) &key)
  (gsll:evaluate-integral
   (spline object)
   (float lower-limit 0d0)
   (float upper-limit 0d0)))

(defgeneric evaluate-tangent (object u)
  (:documentation "Calculates a tangent vector to object at u."))

(defmethod evaluate-tangent ((object plane-curve) (xx real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative x xx))
	   (dy (evaluate-derivative y xx))
	   (mag (norm dx dy)))
      (vector (/ dx mag)
	      (/ dy mag)))))

(defgeneric evaluate-normal (object u)
  (:documentation "Calculates a normal vector to object at u."))

(defmethod evaluate-normal ((object plane-curve) (u real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative x u))
	   (dy (evaluate-derivative y u))
	   (mag (norm dx dy)))
      (vector (/ (- dy) mag)
	      (/ dx mag)))))

(defgeneric evaluate-curvature (object u)
  (:documentation "Calculates the curvature of curve object at u."))

(defmethod evaluate-curvature ((object plane-curve) (u real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative x u))
	   (dy (evaluate-derivative y u))
	   (cx (evaluate-second-derivative x u))
	   (cy (evaluate-second-derivative y u)))
      (/ (- (* dx cy) (* dy cx))
	 (expt (+ (* dx dx) (* dy dy)) 3/2)))))

(defparameter *cparam* 0d0)

(defun multi-lerp (v &rest interpolants)
  (let ((len (length interpolants)))
    (when (< len 2)
      (error "multi-lerp needs two or more interpolants, but got ~a." len))
    (multiple-value-bind (k u) (floor (* v (1- len)))
      (let ((c (nthcdr k interpolants)))
	(lerp u (car c) (cadr c))))))

(defmethod draw ((p particle) (s sdl-screen) &key)
  (with-slots (width height) s
    (let* ((coords (screen-pos p s))
	   (px (car coords))
	   (py (cadr coords)))
      (draw-out-circle px py 1))))

(defmethod draw ((p plane-curve) (s sdl-screen) &key (n 256))
  (with-slots (width height) s
    (with-slots (x y) p
      (let* ((xs (make-array n :element-type 'double-float))
	     (ys (make-array n :element-type 'double-float)))
	(loop :for i :from 0 :below n :do
	   (let* ((u (/ i (1- n)))
		  (xp (param x u))
		  (yp (param y u)))
	     (setf (aref xs i) (evaluate x xp)
		   (aref ys i) (evaluate y yp))))
	(let* ((xmin (loop :for i :from 0 :below n
			:minimizing (aref xs i)))
	       (xmax (loop :for i :from 0 :below n
			:maximizing (aref xs i)))
	       (ymin (loop :for i :from 0 :below n
			:minimizing (aref ys i)))
	       (ymax (loop :for i :from 0 :below n
			:maximizing (aref ys i)))
	       (pmin (min xmin ymin))
	       (pmax (max xmax ymax))
	       (xnew nil)
	       (ynew nil)
	       (xold (screen-pos (aref xs 0) s :min pmin :max pmax))
	       (yold (screen-pos (aref ys 0) s :min pmin :max pmax)))
	  (loop :for i :from 1 :below n :do
	     (setf xnew (screen-pos (aref xs i) s :min pmin :max pmax)
		   ynew (- height (screen-pos (aref ys i) s :min pmin :max pmax)))
	     (draw-out-line xold yold xnew ynew)
	     (setf xold xnew yold ynew)))))))

(defmethod draw ((l spell) (s sdl-screen) &key (clear t))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (parts) l
      (loop :for p :being :the :elements :of parts :do
	 (draw p s)))))

(defun draw-out-circle (x y &optional (hr 1/2))
  (sdl:draw-aa-circle
   (sdl:point :x x :y y)
   (* hr 2)
   :color (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
		     :g (floor (multi-lerp *cparam* 0   0   255 0))
		     :b (floor (multi-lerp *cparam* 0   255 0   0)))))

(defun draw-out-line (x1 y1 x2 y2)
  (setf *cparam* (mod (+ *cparam* 0.001d0) 1d0))
  (sdl-gfx:draw-line-*
   (floor x1) (floor y1) (floor x2) (floor y2)
   :color (sdl:color :r (floor (multi-lerp *cparam* 255 0   0   255))
		     :g (floor (multi-lerp *cparam* 0   0   255 0))
		     :b (floor (multi-lerp *cparam* 0   255 0   0)))))

(defmethod particle-collide ((p rot-particle))
  (with-slots (alive pos vel theta omega) p
    (let ((x (grid:aref pos 0))
	  (y (grid:aref pos 1)))
      ;; Particles die when off-screen.
      (when (or (< x -1d0)
		(> x 1d0)
		(< y -1d0)
		(> y 1d0))
	(setf alive nil)))))

(defmethod update ((p rot-particle) (dt number))
  (with-slots (pos vel acc theta omega) p
    (setf theta (mod (+ theta (* omega dt)) tau)
	  vel (+ vel (* acc dt)))
    (incf (grid:aref pos 0) (* vel dt (cos theta)))
    (incf (grid:aref pos 1) (* vel dt (sin theta)))
    (particle-collide p)))

(defmethod update ((s spell) (dt number))
  ;;(call-next-method s dt)
  (with-slots (pos vel acc theta omega) s
    (setf theta (mod (+ theta (* omega dt)) tau)
	  vel (+ vel (* acc dt)))
    (incf (grid:aref pos 0) (* vel dt (cos theta)))
    (incf (grid:aref pos 1) (* vel dt (sin theta)))
    (particle-collide s))
  (with-slots (parts nups genf) s
    (incf nups)
    (let ((addition (funcall genf s dt)))
      (when addition
	(vector-push-extend addition parts)))
    (loop :for p :being :the :elements :of parts :do
       (update p dt))
    (delete-if-not (lambda (p) (slot-value p 'alive)) parts)))
