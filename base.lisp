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
   (abscissae :accessor abscissae
	      :type 'grid:vector-double-float)
   (ordinates :accessor ordinates
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
	   :type 'gsll:spline)))

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

(defun %resize-data-points% (value object)
  (with-slots (npoints abscissae ordinates) object
    (setf abscissae (grid:make-foreign-array 'double-float :dimensions value))
    (setf ordinates (grid:make-foreign-array 'double-float :dimensions value))
    (setf npoints value)))

(defmethod (setf npoints) ((value integer) (object data-points))
  (%resize-data-points% value object))

(defmethod initialize-instance :after ((d data-points) &key)
  (%resize-data-points% (npoints d) d))

(defmethod initialize-instance :after ((p parametric-points) &key)
  (reparameterize p 'uniform))

(defmethod initialize-instance :after ((s spline-curve) &key)
  (with-slots (spline interpolants) s
    (with-slots (npoints abscissae ordinates start end) interpolants
      (setf spline (gsll:make-spline gsll:+cubic-spline-interpolation+ abscissae ordinates)))))

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
    (let ((longdim (min width height))
	  (factor (/ (sqrt 3))))
      (+ (if shrink (* 1/2 (- 1 factor) longdim) 0)
	 (* (if shrink factor 1)
	    (lerp (/ (- p min) (abs (- max min)))
		  0
		  longdim))))))

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

(defgeneric param (p &optional u)
  (:documentation "Lerps u in [0,1] to p's native domain."))

(defmethod param ((p parametric-points) &optional u)
  (with-slots (start end func) p
    (cond ((not u)
	   (if (eq func #'*) ; TODO: Better test for identity function.
	       1d0
	       (- end start)))
	  ((or (< u 0) (< 1 u))
	   (error "param on ~a was called with ~a not in [0,1]." p u))
	  (t (lerp u start end)))))

(defmethod param ((p interpolation-curve) &optional u)
  (param (interpolants p) u))

(defmethod evaluate ((object parametric-points) (u real) &key)
  (with-slots (func start end) object
    (funcall func (lerp u start end))))

(defmethod evaluate ((object spline-curve) (x real) &key)
  (gsll:evaluate (spline object) (float x 0d0)))

(defun %deriv-h% (x)
  (cond ((plusp x) (* x (sqrt double-float-epsilon)))
	((minusp x) (* x (sqrt double-float-negative-epsilon)))
	(t (expt (* double-float-epsilon double-float-negative-epsilon) 1/4))))

(defmethod evaluate-derivative ((object parametric-points) (u real) &key)
  ;; Numerically calculates derivatives using finite difference methods.
  (with-slots (start end (f func)) object
    (if (or (< u start) (< end u))
	(error "Cannot evaluate derivative of ~a at ~a outside of domain [~a,~a]."
	       object u start end)
	(let* ((h (%deriv-h% u))
	       (2h (* 2 h))
	       (left (abs (- u start)))
	       (right (abs (- u end)))
	       (margin (min left right)))
	  (cond ((>= margin (* 4 h)) ; interval big enough for five-point stencil.
		 (/ (+ (*  1 (funcall f (- u 2h)))
		       (* -8 (funcall f (- u h)))
		       (*  8 (funcall f (+ u h)))
		       (* -1 (funcall f (+ u 2h))))
		    (* 12 h)))
		((>= margin 2h) ; interval big enough for symmetric difference quotient.
		 (/ (+ (* -1 (funcall f (- u h)))
		       (*  1 (funcall f (+ u h))))
		    2h))
		((>= right h) ; interval big enough for right difference quotient.
		 (/ (+ (* -1 (funcall f u))
		       (*  1 (funcall f (+ u h))))
		    h))
		((>= left h) ; interval big enough for left difference quotient.
		 (/ (+ (* -1 (funcall f (- u h)))
		       (*  1 (funcall f u)))
		    h))
		((not (= start end)) ; interval is tiny enough for endpoint secant.
		 (/ (+ (* -1 (funcall f start))
		       (*  1 (funcall f end)))
		    (- end start)))
		(t ; start and end are the same, so there is no change.
		 0d0))))))

(defmethod evaluate-derivative ((object spline-curve) (u real) &key)
  (gsll:evaluate-derivative (spline object) (float u 0d0)))

(defmethod evaluate-derivative ((object plane-curve) (u real) &key)
  (with-slots (x y) object
    (/ (evaluate-derivative* y u)
       (evaluate-derivative* x u))))

(defgeneric evaluate-derivative* (object u &key)
  (:documentation "Differentiates object with u in [0,1] lerped to object's native domain,
    removing the effects of the chain rule.
    E.g. for x=1/2, f'(x) on [0,6] would be f'(6/2) = f'(3), not 6*f'(3) from chain rule."))

(defmethod evaluate-derivative* (object (u real) &key)
  (evaluate-derivative object (param object u)))

(defmethod evaluate-derivative* ((object plane-curve) (u real) &key)
  (evaluate-derivative object u))

(defmethod evaluate-second-derivative ((object spline-curve) (u real) &key)
  (gsll:evaluate-second-derivative (spline object) (float u 0d0)))

(defmethod evaluate-second-derivative ((object plane-curve) (u real) &key)
  (with-slots (x y) object
    (/ (evaluate-second-derivative* y u)
       (evaluate-derivative* x u))))

(defgeneric evaluate-second-derivative* (object u &key)
  (:documentation "Double differentiates object with u in [0,1] lerped to object's native domain,
    removing the effects of the chain rule.
    E.g. for x=1/2, f\"(x) on [0,6] would be f\"(6/2) = f\"(3), not 36*f'(3) from chain rule."))

(defmethod evaluate-second-derivative* (object (u real) &key)
  (evaluate-second-derivative object (param object u)))

(defmethod evaluate-second-derivative* ((object plane-curve) (u real) &key)
  (evaluate-second-derivative object u))

(defmethod evaluate-integral ((object spline-curve)
			      (lower-limit real)
			      (upper-limit real) &key)
  (gsll:evaluate-integral
   (spline object)
   (float lower-limit 0d0)
   (float upper-limit 0d0)))

(defgeneric evaluate-tangent (object u)
  (:documentation "Calculates a tangent vector to object at u."))

(defmethod evaluate-tangent ((object plane-curve) (u real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative* x u))
	   (dy (evaluate-derivative* y u))
	   (mag (norm dx dy)))
      (vector (/ dx mag)
	      (/ dy mag)))))

(defgeneric evaluate-normal (object u)
  (:documentation "Calculates a normal vector to object at u."))

(defmethod evaluate-normal ((object plane-curve) (u real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative* x u))
	   (dy (evaluate-derivative* y u))
	   (mag (norm dx dy)))
      (vector (/ (- dy) mag)
	      (/ dx mag)))))

(defgeneric evaluate-curvature (object u)
  (:documentation "Calculates the curvature of curve object at u."))

(defmethod evaluate-curvature ((object plane-curve) (u real))
  (with-slots (x y) object
    (let* ((dx (evaluate-derivative* x u))
	   (dy (evaluate-derivative* y u))
	   (cx (evaluate-second-derivative* x u))
	   (cy (evaluate-second-derivative* y u)))
      (/ (- (* dx cy) (* dy cx))
	 (expt (+ (* dx dx) (* dy dy)) 3/2)))))

(defgeneric arc-length (curve u &optional v)
  (:documentation "Calculates the arc length of curve from 0 to u,
or between u and v if v is supplied. u and v are in [0,1]."))

(defmethod arc-length ((object parametric-points) (u double-float) &optional v)
  (let ((a (if v u 0d0))
	(b (or v u))
	(ds (lambda (u) (sqrt (+ 1d0 (expt (evaluate-derivative object u) 2))))))
    (gsll:integration-qag ds a b :gauss61)))

(defmethod arc-length ((object spline-curve) (u double-float) &optional v)
  (let ((a (if v u 0d0))
	(b (or v u))
	(ds (lambda (u) (sqrt (+ 1d0 (expt (evaluate-derivative object u) 2))))))
    (gsll:integration-qag ds a b :gauss61)))

(defmethod arc-length ((object plane-curve) (u double-float) &optional v)
  (with-slots (x y) object
    (let ((a (if v u 0d0))
	  (b (or v u))
	  (ds (lambda (u*) (* (param x)
			 (param y)
			 (sqrt (+ (expt (evaluate-derivative* x u*) 2)
				  (expt (evaluate-derivative* y u*) 2)))))))
      (gsll:integration-qag ds a b :gauss61))))

(defgeneric arc-param (object s)
  (:documentation "Finds the parameter where the arc length of object is s."))

(defmethod arc-param ((object parametric-points) (s double-float))
  (with-slots (start end) object
    (let ((max-iter 64)
	  (solver
	   (gsll:make-one-dimensional-root-solver-f
	    gsll:+brent-fsolver+
	    (lambda (u) (- (arc-length object u) s))
	    start
	    end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 1d-3)))
	 :finally (return root)))))

(defmethod arc-param ((object spline-curve) (s double-float))
  (with-slots (start end) (interpolants object)
    (let ((max-iter 64)
	  (solver
	   (gsll:make-one-dimensional-root-solver-f
	    gsll:+brent-fsolver+
	    (lambda (u) (- (arc-length object u) s))
	    start
	    end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 1d-3)))
	 :finally (return root)))))

(defmethod arc-param ((object plane-curve) (s double-float))
  (with-slots (x y) object
    (let* ((max-iter 64)
	   ;(xinterp (interpolants x))
	   ;(yinterp (interpolants y))
	   ;(start (max (start xinterp) (start yinterp)))
	   ;(end (min (end xinterp) (end yinterp)))
	   (start 0d0)
	   (end 1d0)
	   (solver
	    (gsll:make-one-dimensional-root-solver-f
	     gsll:+brent-fsolver+
	     (lambda (u) (- (arc-length object u) s))
	     start
	     end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 1d-3)))
	 :finally (return root)))))

(defgeneric reparameterize (object method)
  (:documentation "Reparametrizes object with method. Supported methods are:
    UNIFORM : Constant steps in the parameter.
    ARC-LENGTH : Constant steps in arc length."))

(defmethod reparameterize :before ((object parametric-points) (method (eql 'uniform)))
  (with-slots (npoints abscissae start end func) object
    (loop :for i :from 0 :below npoints :do
       (setf (grid:aref abscissae i) (lerp (/ i (1- npoints)) start end)))))

(defmethod reparameterize :before ((object parametric-points) (method (eql 'arc-length)))
  (with-slots (npoints abscissae start end func) object
    (let ((l (arc-length object start end)))
      (loop :for i :from 0 :below npoints :do
	 (setf (grid:aref abscissae i) (arc-param object (* l (/ i (1- npoints)))))))))

(defmethod reparameterize ((object parametric-points) method)
  (with-slots (npoints abscissae ordinates func) object
    (loop :for i :from 0 :below npoints :do
       (setf (grid:aref ordinates i) (funcall func (grid:aref abscissae i))))
    object))

(defmethod reparameterize ((object interpolation-curve) method)
  (reparameterize (interpolants object) method))

(defmethod reparameterize ((object spline-curve) method)
  (with-slots (spline interpolants) object
    (with-slots (abscissae ordinates) interpolants
      (call-next-method)
      (setf spline (gsll:make-spline gsll:+cubic-spline-interpolation+ abscissae ordinates)))))

(defmethod reparameterize :before ((object plane-curve) method)
  (with-slots (x y) object
    (with-accessors ((xpoints npoints)) (interpolants x)
      (with-accessors ((ypoints npoints)) (interpolants y)
	(cond ((< xpoints ypoints)
	       (setf xpoints ypoints))
	      ((> xpoints ypoints)
	       (setf ypoints xpoints)))))))

(defmethod reparameterize ((object plane-curve) (method (eql 'uniform)))
  (reparameterize (x object) method)
  (reparameterize (y object) method)
  object)

(defmethod reparameterize ((object plane-curve) (method (eql 'arc-length)))
  (with-slots (x y) object
    (with-slots ((xis interpolants)) x
      (with-slots ((yis interpolants)) y
	(with-slots ((xabsc abscissae)) xis
	  (with-slots ((yabsc abscissae) npoints) yis
	    (let ((l (arc-length object 0d0 1d0)))
	      (loop :for i :from 0 :below npoints :do
		 (let ((abscissa (arc-param object (* l (/ i (1- npoints))))))
		   (setf (grid:aref xabsc i) (param xis abscissa))
		   (setf (grid:aref yabsc i) (param yis abscissa))))
	      (reparameterize x nil)
	      (reparameterize y nil)
	      object)))))))

(defparameter *cparam* 0d0)

(defun multi-lerp (v &rest interpolants)
  (let ((len (length interpolants)))
    (when (< len 2)
      (error "multi-lerp needs two or more interpolants, but got ~a." len))
    (multiple-value-bind (k u) (floor (* v (1- len)))
      (let ((c (nthcdr k interpolants)))
	(lerp u (car c) (cadr c))))))

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

(defgeneric draw (p s &key)
  (:documentation "Draws the particle p on the screen s."))

(defmethod draw ((p particle) (s sdl-screen) &key)
  (with-slots (width height) s
    (let* ((coords (screen-pos p s))
	   (px (car coords))
	   (py (cadr coords)))
      (draw-out-circle px py 1))))

(defmethod draw ((p plane-curve) (s sdl-screen) &key (n 256) (base t))
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
	     (setf xold xnew yold ynew))
	  (when base
	    (let* ((xis (interpolants x))
		   (yis (interpolants y))
		   (xod (ordinates xis))
		   (yod (ordinates yis))
		   (nx (npoints xis))
		   (ny (npoints yis)))
	      (if (not (= nx ny))
		  (error "Components of ~a have different npoints: ~a and ~a"
			 p nx ny)
		  (loop :for i :from 0 :below (min nx ny) :do
		     (draw-out-circle (screen-pos (grid:aref xod i) s :min pmin :max pmax)
				      (- height
					 (screen-pos (grid:aref yod i) s :min pmin :max pmax))
				      1))))))))))

(defmethod draw ((l spell) (s sdl-screen) &key (clear t))
  (when clear
    (sdl:clear-display (sdl:color)))
  (with-slots (width height) s
    (with-slots (parts) l
      (loop :for p :being :the :elements :of parts :do
	 (draw p s)))))

(defgeneric particle-collide (p)
  (:documentation "Collides the particle with the boundaries."))

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

(defgeneric update (p dt)
  (:documentation "Updates the moving object p with time step dt."))

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
