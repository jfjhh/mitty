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
	   :type 'grid:vector-double-float)))

(defclass parametric-points (data-points)
  ((func :initarg :func
	 :accessor func
	 :type 'function)
   (start :initarg :start
	  :accessor start
	  :type 'real)
   (end :initarg :end
	:accessor end
	:type 'real)))

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

(defmethod initialize-instance :after ((d data-points) &key)
  (with-slots (npoints points) d
    (setf points (grid:make-foreign-array 'double-float :dimensions npoints))))

(defmethod initialize-instance :after ((p parametric-points) &key)
  (with-slots (npoints points func start end) p
    (loop :for i :from 0 :below npoints :do
       (setf (grid:aref points i) (funcall func (lerp (/ i (1- npoints)) start end))))))

(defmethod initialize-instance :after ((s spline-curve) &key)
  (with-slots (spline ducks interpolants) s
    (with-slots (npoints points) interpolants
      (setf ducks (grid:make-foreign-array 'double-float :dimensions npoints))
      (loop :for i :from 0 :below npoints :do ;; Uniform spline ducks.
	 (setf (grid:aref ducks i) (float (/ i (1- npoints)) 0d0)))
      (setf spline (gsll:make-spline gsll:+cubic-spline-interpolation+ ducks points)))))

(defgeneric generate (g dimfuncs &key start end)
  (:documentation "Generates g with the vector results of dimfuncs on [0,1]."))

(defgeneric draw (p s &key)
  (:documentation "Draws the particle p on the screen s."))

(defgeneric particle-collide (p)
  (:documentation "Collides the particle with the boundaries."))

(defgeneric update (p dt)
  (:documentation "Updates the moving object p with time step dt."))

(defmethod evaluate ((object spline-curve) (point real) &key)
  (gsll:evaluate (spline object) (float point 0d0)))

(defmethod evaluate-derivative ((object spline-curve) (point real) &key)
  (gsll:evaluate-derivative (spline object) (float point 0d0)))

(defmethod evaluate-second-derivative ((object spline-curve) (point real) &key)
  (gsll:evaluate-second-derivative (spline object) (float point 0d0)))

(defmethod evaluate-integral ((object spline-curve)
			      (lower-limit real)
			      (upper-limit real) &key)
  (gsll:evaluate-integral (spline object) (float lower-limit 0d0) (float upper-limit 0d0)))

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

(defmethod draw ((g generator) (s sdl-screen) &key (hr 5) (pmin nil) (pmax nil))
  (with-slots (width height) s
    (with-slots (dims ndims npoints) g
      (let* ((coords
	      (map
	       'vector
	       (lambda (d)
		 (unless pmin
		   (setf pmin
			 (loop :for i :from 0 :below npoints
			    :minimizing (grid:aref d i))))
		 (unless pmax
		   (setf pmax
			 (loop :for i :from 0 :below npoints
			    :maximizing (grid:aref d i))))
		 (grid:map-grid :source d
				:element-function
				(lambda (x) (screen-pos x s :min pmin :max pmax))))
	       dims)))
	(setf *cparam* (mod (+ *cparam* 0.005d0) 1d0))
	(loop :for i :from 0 :below npoints :do
	   (let ((cs (loop :for j :from 0 :below 2
			:collecting
			(let ((coord (grid:aref (aref coords j) i)))
			  (if (= j 1) (- height coord) coord)))))
	     (draw-out-circle (car cs) (cadr cs) hr)))))))

(defmethod draw ((p plane-curve) (s sdl-screen) &key (n 256))
  (with-slots (width height) s
    (with-slots (x y) p
      (let* ((xs (make-array n :element-type 'double-float))
	     (ys (make-array n :element-type 'double-float)))
	(loop :for i :from 0 :below n :do
	   (setf (aref xs i) (evaluate x (/ i (1- n))))
	   (setf (aref ys i) (evaluate y (/ i (1- n)))))
	(setf tmp0 (list xs ys))
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
	     (setf xnew (screen-pos (aref xs i) s :min pmin :max pmax))
	     (setf ynew (- height (screen-pos (aref ys i) s :min pmin :max pmax)))
	     (draw-out-line xold yold xnew ynew)
	     (setf xold xnew)
	     (setf yold ynew)))))))

(defmethod draw ((p splinerator) (s sdl-screen)
		 &key (hr 1/2) (base nil) (clear nil) (pmin nil) (pmax nil))
  (declare (ignore hr))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (sdims ddims cdims ndims nevals) p
      (let* ((coords
	      (map
	       'vector
	       (lambda (d)
		 (unless pmin
		   (setf pmin
			 (loop :for i :from 0 :below nevals
			    :minimizing (grid:aref d i))))
		 (unless pmax
		   (setf pmax
			 (loop :for i :from 0 :below nevals
			    :maximizing (grid:aref d i))))
		 (grid:map-grid :source d
				:element-function
				(lambda (x) (screen-pos x s :min pmin :max pmax))))
	       sdims))
	     (oldcs (loop :for j :from 0 :below 2
		       :collecting
		       (let ((coord (grid:aref (aref coords j) 0)))
			 (if (= j 1) (- height coord) coord)))))
	(loop :for i :from 1 :below nevals :do
	   (let* ((newcs
		   (loop :for j :from 0 :below 2
		      :collecting
		      (let ((coord (grid:aref (aref coords j) i)))
			(if (= j 1) (- height coord) coord))))
		  (x0 (car  oldcs))
		  (y0 (cadr oldcs))
		  (x1 (car  newcs))
		  (y1 (cadr newcs))
		  (dx (grid:aref (aref ddims 0) i))
		  (dy (grid:aref (aref ddims 1) i))
		  (cx (grid:aref (aref cdims 0) i))
		  (cy (grid:aref (aref cdims 1) i))
		  (vq (+ (* dx dx) (* dy dy)))
		  (k (/ (- (* dx cy) (* dy cx)) (expt vq 3/2)))
		  ;;(tn (atan dx dy))
		  (mag (norm dx dy))
		  (nx (/ (- dy) mag))
		  (ny (/ dx mag))
		  (nlen (* 16 k))
		  ;;(xn (+ x1 (* nlen (cos tn))))
		  ;;(yn (+ y1 (* nlen (sin tn))))
		  (xn (+ x1 (* nlen nx)))
		  (yn (+ y1 (* nlen (- ny)))))
	     (draw-out-line x0 y0 x1 y1)
	     (draw-out-line x1 y1 xn yn)
	     (setf oldcs newcs)))
	(when base
	  (call-next-method p s :hr 2 :pmin pmin :pmax pmax))))))

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

(defgeneric evaluate-tangent (object point)
  (:documentation "Calculates a tangent vector to object at point."))

(defmethod evaluate-tangent ((object splinerator) (point real))
  (with-slots (gsplines) object
    (let* ((xspline (aref gsplines 0))
	   (yspline (aref gsplines 1))
	   (dx (gsll:evaluate-derivative xspline point))
	   (dy (gsll:evaluate-derivative yspline point))
	   (mag (norm dx dy)))
      (vector (/ dx mag)
	      (/ dy mag)))))

(defgeneric evaluate-normal (object point)
  (:documentation "Calculates a normal vector to object at point."))

(defmethod evaluate-normal ((object splinerator) (point real))
  (with-slots (gsplines) object
    (let* ((xspline (aref gsplines 0))
	   (yspline (aref gsplines 1))
	   (dx (gsll:evaluate-derivative xspline point))
	   (dy (gsll:evaluate-derivative yspline point))
	   (mag (norm dx dy)))
      (vector (/ (- dy) mag)
	      (/ dx mag)))))

(declaim (inline vec-slope))
(defun vec-slope (vec)
  (declare (optimize (speed 3) (safety 1))
	   (type (simple-vector 2) vec))
  (let ((x (aref vec 0))
	(y (aref vec 1)))
    (declare (type double-float x y))
    (atan y x)))
(declaim (notinline vec-slope))

(defgeneric evaluate-curvature (object point)
  (:documentation "Calculates the curvature of curve object at point."))

(defmethod evaluate-curvature ((object splinerator) (point real))
  (with-slots (gsplines) object
    (let* ((xspline (aref gsplines 0))
	   (yspline (aref gsplines 1))
	   (dx (gsll:evaluate-derivative xspline point))
	   (dy (gsll:evaluate-derivative yspline point))
	   (cx (gsll:evaluate-second-derivative xspline point))
	   (cy (gsll:evaluate-second-derivative yspline point)))
      (/ (- (* dx cy) (* dy cx))
	 (expt (+ (* dx dx) (* dy dy)) 3/2)))))

(defmethod draw ((l spell) (s sdl-screen) &key (clear t))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (parts) l
      (loop :for p :being :the :elements :of parts :do
	 (draw p s)))))

(defun draw-out-circle (x y &optional (hr 1/2))
  ;;(setf *cparam* (mod (+ *cparam* 0.0001d0) 1d0))
  ;;(sdl:draw-filled-circle
   (sdl:draw-aa-circle
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
  (with-slots (alive pos vel theta omega) p
    (let ((x (grid:aref pos 0))
	  (y (grid:aref pos 1)))
      #||
      ;; Wall wrapping.
      (when (< x -1d0)
	(setf (grid:aref pos 0)  1d0))
      (when (> x 1d0)
	(setf (grid:aref pos 0) -1d0))
      (when (< y -1d0)
	(setf (grid:aref pos 1)  1d0))
      (when (> y 1d0)
	(setf (grid:aref pos 1) -1d0))
      ||#

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

      ;; Dying.
      (when (or (< x -1d0)
		(> x 1d0)
		(< y -1d0)
		(> y 1d0))
	(setf alive nil))
      )))

(defmethod update ((p rot-particle) (dt number))
  (with-slots (pos vel acc theta omega) p
    (setf theta (mod (+ theta (* omega dt)) tau))
    (setf vel (+ vel (* acc dt)))
    (incf (grid:aref pos 0) (* vel dt (cos theta)))
    (incf (grid:aref pos 1) (* vel dt (sin theta)))
    (particle-collide p)))

(defmethod update ((s spell) (dt number))
  ;;(call-next-method s dt)
  (with-slots (pos vel acc theta omega) s
    (setf theta (mod (+ theta (* omega dt)) tau))
    (setf vel (+ vel (* acc dt)))
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
