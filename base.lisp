;;;;
;;;; The kitchen sink for particles, splines, screens, etc.
;;;; Better organization sometime later.
;;;;
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))

(defconstant tau (* 2d0 pi))
(declaim (type double-float tau))

(defparameter +arc-abs-error+ (* gsll:*default-absolute-error* 1.d2))
(defparameter +arc-rel-error+ 1d-3)

(defclass screen ()
  ((dims :initarg :dims
	 :accessor dims
	 :type 'vector-double-float)))

(defclass sdl-screen ()
  ((width :initarg :width
	  :accessor width
	  :type 'double-float)
   (height :initarg :height
	   :accessor height
	   :type 'double-float)))

(defclass particle ()
  ((alive :initform t
	  :accessor alive
	  :type '(boolean null))
   (pos :initform (make-foreign-array
		   'double-float
		   :initial-contents '(0d0 0d0))
	:initarg :pos
	:accessor pos
	:type 'vector-double-float)))

(defclass data-points ()
  ((npoints :initarg :npoints
	    :accessor npoints
	    :type 'integer)
   (abscissae :accessor abscissae
	      :type 'vector-double-float)
   (ordinates :accessor ordinates
	      :type 'vector-double-float)
   (start :initarg :start
	  :accessor start
	  :type 'double-float)
   (end :initarg :end
	:accessor end
	:type 'double-float)))

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
    (setf abscissae (make-foreign-array 'double-float :dimensions value))
    (setf ordinates (make-foreign-array 'double-float :dimensions value))
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

(defun %promote-numbers% (sexp)
  (cond ((realp sexp) (float sexp 0d0))
	((listp sexp) (mapcar #'%promote-numbers% sexp))
	(t sexp)))

(defun %annotate-calls% (sexp)
  (cond ((listp sexp) `(the double-float ,(mapcar #'%annotate-calls% sexp)))
	(t sexp)))

(defmacro as-param (body &key (from 0d0) (to 1d0) (n 64) (wrt 'u))
  (if (eq (car body) 'as-param)
      body
      (let ((sym (gensym "PARAM")))
	(labels ((%alpha-conversion% (sexp)
		   (cond ((eq sexp wrt) sym)
			 ((listp sexp) (mapcar #'%alpha-conversion% sexp))
			 (t sexp))))
	  `(make-instance 'parametric-points
			  :start ,(%promote-numbers% from)
			  :end ,(%promote-numbers% to)
			  :npoints ,n
			  :func (lambda (,sym)
				  (declare (type (double-float 0d0) ,sym)
					   (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))
				  ,(%annotate-calls%
				    (%promote-numbers%
				     (%alpha-conversion% body)))))))))

(defmacro as-spline (&body body)
  (if (eq (caar body) 'as-spline)
      (car body)
      `(make-instance 'spline-curve
		      :interpolants (as-param ,@body))))

(defun %split-args% (arg-list &optional prev)
  "Destructures a list of argument lists into individual argument lists."
  (let ((s (car arg-list))
	(r (cdr arg-list)))
    (cond ((and (not (keywordp s))
		(not (keywordp (car r))))
	   (cons (nreverse (cons s prev))
		 (when (not (null r))
		   (%split-args% r))))
	  ((not (null arg-list))
	   (%split-args% r (cons s prev))))))

(defmacro as-plane (&rest args)
  (let* ((split-args (%split-args% args))
	 (x (car split-args))
	 (y (cadr split-args)))
    `(make-instance 'plane-curve
		    :x (as-spline ,@x)
		    :y (as-spline ,@y))))

(defgeneric screen-pos (p s &key)
  (:documentation "Maps the particle coordinates of p to screen coordinates on s."))

(defmethod screen-pos ((p particle) (s sdl-screen) &key)
  (with-slots (pos) p
    (with-slots (width height) s
      (let ((px (aref pos 0))
	    (py (aref pos 1)))
	(declare (type double-float px py width height))
	(list (lerp (/ (+ px 1d0) 2d0) 0d0 width)
	      (lerp (- 1d0 (/ (+ py 1d0) 2d0)) 0d0 height))))))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key)
  (with-slots (width height) s
    (declare (type double-float width height))
    (let ((longdim (min width height)))
      (declare (type double-float longdim))
      (lerp (/ (+ p 1d0) 2d0) 0d0 longdim))))

(defmethod screen-pos ((p double-float) (s sdl-screen) &key min max (shrink t))
  (with-slots (width height) s
    (declare (type double-float width height min max))
    (let ((longdim (min width height))
	  (factor (/ (sqrt 3d0))))
      (declare (type double-float longdim))
      (+ (if shrink (* 0.5d0 (- 1d0 factor) longdim) 0d0)
	 (* (if shrink factor 1d0)
	    (lerp (/ (- p min) (abs (- max min)))
		  0d0
		  longdim))))))

(declaim (inline quadrance))
(defun quadrance (&rest nums)
  (declare (optimize (speed 3) (safety 1)))
  (reduce (lambda (x y)
	    (declare (type double-float x y))
	    (+ x (* y y))) nums :initial-value 0d0))

(declaim (inline norm))
(defun norm (&rest nums)
  (declare (optimize (speed 3) (safety 1))
	   (inline quadrance sqrt))
  (sqrt (the (double-float 0d0) (apply #'quadrance nums))))

(declaim (inline vec-slope))
(defun vec-slope (vec)
  (declare (optimize (speed 3) (safety 1))
	   (type (simple-vector 2) vec))
  (let ((x (aref vec 0))
	(y (aref vec 1)))
    (declare (type double-float x y))
    (atan y x)))

(defgeneric param (p &optional u)
  (:documentation "Lerps u in [0,1] to p's native domain."))

(declaim (inline param))
(defmethod param ((p parametric-points) &optional u)
  (with-slots (start end func) p
    (declare (type double-float start end))
    (cond ((not u)
	   (if (eq func #'*) ; TODO: Better test for identity function.
	       1d0
	       (- end start)))
	  ((or (< (the double-float u) 0d0) (< 1d0 (the double-float u)))
	   (error "param on ~a was called with ~a not in [0,1]." p u))
	  (t (lerp (the double-float u) start end)))))

(defmethod param ((p interpolation-curve) &optional u)
  (param (interpolants p) u))

(defmethod evaluate ((object parametric-points) (u double-float) &key)
  (with-slots (func start end) object
    (declare (type double-float start end)
	     (type (function (double-float) double-float) func))
    (funcall func (lerp u start end))))

(defmethod evaluate ((object spline-curve) (x double-float) &key)
  (gsll:evaluate (spline object) (float x 0d0)))

(defun %deriv-h% (x)
  (declare (type double-float x))
  (cond ((plusp x) (* x (sqrt double-float-epsilon)))
	((minusp x) (* x (sqrt double-float-negative-epsilon)))
	(t (expt (* double-float-epsilon double-float-negative-epsilon) 1/4))))

(defmethod evaluate-derivative ((object parametric-points) (u double-float) &key)
  "Calculates the derivative of object at u using finite difference methods."
  (with-slots (start end (f func)) object
    (declare (type double-float start end u)
	     (type (function (double-float) double-float) f))
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


(defmethod evaluate-derivative ((object spline-curve) (u double-float) &key)
  (gsll:evaluate-derivative (spline object) (float u 0d0)))

(defmethod evaluate-derivative ((object plane-curve) (u double-float) &key)
  (with-slots (x y) object
    (/ (the double-float (evaluate-derivative* y u))
       (the double-float (evaluate-derivative* x u)))))

(defgeneric evaluate-derivative* (object u &key)
  (:documentation "Differentiates object with u in [0,1] lerped to object's native domain,
    removing the effects of the chain rule.
    E.g. for x=1/2, f'(x) on [0,6] would be f'(6/2) = f'(3), not 6*f'(3) from chain rule."))

(defmethod evaluate-derivative* (object (u double-float) &key)
  (evaluate-derivative object (param object u)))

(defmethod evaluate-derivative* ((object plane-curve) (u double-float) &key)
  (evaluate-derivative object u))

(defmethod evaluate-second-derivative ((object spline-curve) (u double-float) &key)
  (gsll:evaluate-second-derivative (spline object) (float u 0d0)))

(defmethod evaluate-second-derivative ((object plane-curve) (u double-float) &key)
  (with-slots (x y) object
    (/ (the double-float (evaluate-second-derivative* y u))
       (the double-float (evaluate-derivative* x u)))))

(defgeneric evaluate-second-derivative* (object u &key)
  (:documentation "Double differentiates object with u in [0,1] lerped to object's native domain,
    removing the effects of the chain rule.
    E.g. for x=1/2, f\"(x) on [0,6] would be f\"(6/2) = f\"(3), not 36*f'(3) from chain rule."))

(defmethod evaluate-second-derivative* (object (u double-float) &key)
  (evaluate-second-derivative object (param object u)))

(defmethod evaluate-second-derivative* ((object plane-curve) (u double-float) &key)
  (evaluate-second-derivative object u))

(defmethod evaluate-integral ((object spline-curve)
			      (lower-limit double-float)
			      (upper-limit double-float) &key)
  (gsll:evaluate-integral
   (spline object)
   (float lower-limit 0d0)
   (float upper-limit 0d0)))

(defgeneric evaluate-tangent (object u)
  (:documentation "Calculates a tangent vector to object at u."))

(defmethod evaluate-tangent ((object plane-curve) (u double-float))
  (with-slots (x y) object
    (let* ((dx (the double-float (evaluate-derivative* x u)))
	   (dy (the double-float (evaluate-derivative* y u)))
	   (mag (norm dx dy)))
      (vector (/ dx mag)
	      (/ dy mag)))))

(defgeneric evaluate-normal (object u)
  (:documentation "Calculates a normal vector to object at u."))

(defmethod evaluate-normal ((object plane-curve) (u double-float))
  (with-slots (x y) object
    (let* ((dx (the double-float (evaluate-derivative* x u)))
	   (dy (the double-float (evaluate-derivative* y u)))
	   (mag (norm dx dy)))
      (vector (/ (- dy) mag)
	      (/ dx mag)))))

(defgeneric evaluate-curvature (object u)
  (:documentation "Calculates the curvature of curve object at u."))

(defmethod evaluate-curvature ((object plane-curve) (u double-float))
  (with-slots (x y) object
    (let* ((dx (the double-float (evaluate-derivative* x u)))
	   (dy (the double-float (evaluate-derivative* y u)))
	   (cx (the double-float (evaluate-second-derivative* x u)))
	   (cy (the double-float (evaluate-second-derivative* y u))))
      (/ (- (* dx cy) (* dy cx))
	 (the double-float (expt (+ (* dx dx) (* dy dy)) 3/2))))))

(defgeneric arc-length (curve u &optional v)
  (:documentation "Calculates the arc length of curve from 0 to u,
or between u and v if v is supplied. u and v are in [0,1]."))

(defmethod arc-length ((object parametric-points) (u double-float) &optional v)
  (let ((a (if v u 0d0))
	(b (or v u))
	(ds (lambda (u) (sqrt (+ 1d0 (expt (the double-float (evaluate-derivative object u)) 2))))))
    (gsll:integration-qag ds a b :gauss15 +arc-abs-error+ +arc-rel-error+)))

(defmethod arc-length ((object spline-curve) (u double-float) &optional v)
  (let ((a (if v u 0d0))
	(b (or v u))
	(ds (lambda (u) (sqrt (+ 1d0 (expt (the double-float (evaluate-derivative object u)) 2))))))
    (gsll:integration-qag ds a b :gauss15 +arc-abs-error+ +arc-rel-error+)))

(defmethod arc-length ((object plane-curve) (u double-float) &optional v)
  (with-slots (x y) object
    (let ((a (if v u 0d0))
	  (b (or v u))
	  (ds (lambda (u*) (* (the double-float (param x))
			      (the double-float (param y))
			      (sqrt (+ (expt (the double-float (evaluate-derivative* x u*)) 2)
				       (expt (the double-float (evaluate-derivative* y u*)) 2)))))))
      (gsll:integration-qag ds a b :gauss15 +arc-abs-error+ +arc-rel-error+))))

(defgeneric arc-param (object s)
  (:documentation "Finds the parameter where the arc length of object is s."))

(defmethod arc-param ((object parametric-points) (s double-float))
  (with-slots (start end) object
    (let ((max-iter 64)
	  (solver
	   (gsll:make-one-dimensional-root-solver-f
	    gsll:+brent-fsolver+
	    (lambda (u) (- (the double-float (arc-length object u)) s))
	    start
	    end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 +arc-abs-error+)))
	 :finally (return root)))))

(defmethod arc-param ((object spline-curve) (s double-float))
  (with-slots (start end) (interpolants object)
    (let ((max-iter 64)
	  (solver
	   (gsll:make-one-dimensional-root-solver-f
	    gsll:+brent-fsolver+
	    (lambda (u) (- (the double-float (arc-length object u)) s))
	    start
	    end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 +arc-abs-error+)))
	 :finally (return root)))))

(defmethod arc-param ((object plane-curve) (s double-float))
  (with-slots (x y) object
    (let* ((max-iter 64)
	   (start 0d0)
	   (end 1d0)
	   (solver
	    (gsll:make-one-dimensional-root-solver-f
	     gsll:+brent-fsolver+
	     (lambda (u) (- (the double-float (arc-length object u)) s))
	     start
	     end)))
      (loop :for iter :from 0
	 :for root = (gsll:solution solver)
	 :for lower = (gsll:fsolver-lower solver)
	 :for upper = (gsll:fsolver-upper solver)
	 :do (gsll:iterate solver)
	 :while (and (< iter max-iter)
		     (not (gsll:root-test-interval lower upper 0d0 +arc-abs-error+)))
	 :finally (return root)))))

(defgeneric reparameterize (object method)
  (:documentation "Reparametrizes object with method. Supported methods are:
    UNIFORM : Constant steps in the parameter.
    ARC-LENGTH : Constant steps in arc length."))

(defmethod reparameterize :before ((object parametric-points) (method (eql 'uniform)))
  (with-slots (npoints abscissae start end func) object
    (declare (type fixnum npoints)
	     (type double-float start end)
	     (type (function (double-float) double-float) func))
    (loop :for i :from 0 :below npoints :do
       (setf (aref abscissae i) (lerp (/ i (- npoints 1d0)) start end)))))

(defmethod reparameterize :before ((object parametric-points) (method (eql 'arc-length)))
  (with-slots (npoints abscissae start end func) object
    (declare (type fixnum npoints)
	     (type double-float start end)
	     (type (function (double-float) double-float) func))
    (let ((l (the double-float (arc-length object start end))))
      (loop :for i :from 0 :below npoints :do
	 (setf (aref abscissae i) (arc-param object (* l (/ i (- npoints 1d0)))))))))

(defmethod reparameterize ((object parametric-points) method)
  (with-slots (npoints abscissae ordinates func) object
    (declare (type fixnum npoints)
	     (type (function (double-float) double-float) func))
    (loop :for i :from 0 :below npoints :do
       (setf (aref ordinates i) (funcall func (aref abscissae i))))
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
	(declare (type fixnum xpoints ypoints))
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
	    (declare (type fixnum npoints))
	    (let ((l (the double-float (arc-length object 0d0 1d0))))
	      (loop :for i :from 0 :below npoints :do
		 (let ((abscissa (the double-float
				      (arc-param object (* l (/ i (- npoints 1d0)))))))
		   (setf (aref xabsc i) (param xis abscissa))
		   (setf (aref yabsc i) (param yis abscissa))))
	      (reparameterize x nil)
	      (reparameterize y nil)
	      object)))))))

(defparameter *cparam* 0d0)
(declaim (type double-float *cparam*))

(defun multi-lerp (v &rest interpolants)
  (declare (type double-float v))
  (let ((len (the fixnum (length interpolants))))
    (when (< len 2)
      (error "multi-lerp needs two or more interpolants, but got ~a." len))
    (multiple-value-bind (k u) (floor (* v (1- len)))
      (let ((c (nthcdr k interpolants)))
	(lerp u (float (car c) 0d0) (float (cadr c) 0d0))))))

(defun draw-out-circle (x y &optional (hr 0.5d0))
  (sdl:draw-aa-circle
   (sdl:point :x x :y y)
   (floor (* hr 2))
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

(defmethod draw :before (p (s sdl-screen) &key (clear t))
  (when clear
    (sdl:clear-display (sdl:color))))

(defmethod draw ((p particle) (s sdl-screen) &key)
  (with-slots (width height) s
    (let* ((coords (screen-pos p s))
	   (px (car coords))
	   (py (cadr coords)))
      (draw-out-circle px py 1))))

(defmethod draw ((p plane-curve) (s sdl-screen) &key (n 1024) (base t))
  (with-slots (width height) s
    (with-slots (x y) p
      (let* ((xs (make-array n :element-type 'double-float))
	     (ys (make-array n :element-type 'double-float))
	     (ns (make-array n :element-type '(simple-vector 2))))
	(loop :for i :from 0 :below n :do
	   (let* ((u (/ i (- n 1d0)))
		  (xp (param x u))
		  (yp (param y u)))
	     (setf (aref xs i) (the double-float (evaluate x xp))
		   (aref ys i) (the double-float (evaluate y yp))
		   (aref ns i) (evaluate-normal p u))))
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
		    (nlen (* 12 (evaluate-curvature p (/ i (- n 1d0))))))
	       (setf xnew (screen-pos (aref xs i) s :min pmin :max pmax)
		     ynew (- height (screen-pos (aref ys i) s :min pmin :max pmax)))
	       (draw-out-line xold yold xnew ynew)
	       (draw-out-line xnew ynew (+ xnew (* nlen xnorm)) (+ ynew (* -1 nlen ynorm)))
	       (setf xold xnew yold ynew)))
	  (when base
	    (let* ((xis (interpolants x))
		   (yis (interpolants y))
		   (xod (ordinates xis))
		   (yod (ordinates yis))
		   (nx (the fixnum (npoints xis)))
		   (ny (the fixnum (npoints yis))))
	      (if (not (= nx ny))
		  (error "Components of ~a have different npoints: ~a and ~a"
			 p nx ny)
		  (loop :for i :from 0 :below (min nx ny) :do
		     (draw-out-circle (screen-pos (aref xod i) s :min pmin :max pmax)
				      (- height
					 (screen-pos (aref yod i) s :min pmin :max pmax))
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
    (let ((x (the double-float (aref pos 0)))
	  (y (the double-float (aref pos 1))))
      ;; Particles die when off-screen.
      (when (or (< x -1d0)
		(> x 1d0)
		(< y -1d0)
		(> y 1d0))
	(setf alive nil)))))

(defgeneric update (p dt)
  (:documentation "Updates the moving object p with time step dt."))

(defmethod update ((p rot-particle) (dt double-float))
  (with-slots (pos vel acc theta omega) p
    (declare (type double-float vel acc theta omega))
    (setf theta (mod (+ theta (* omega dt)) tau)
	  vel (+ vel (* acc dt)))
    (incf (aref pos 0) (* vel dt (cos theta)))
    (incf (aref pos 1) (* vel dt (sin theta)))
    (particle-collide p)))

(defmethod update ((s spell) (dt double-float))
  (with-slots (pos vel acc theta omega) s
    (declare (type double-float vel acc theta omega))
    (setf theta (mod (+ theta (* omega dt)) tau)
	  vel (+ vel (* acc dt)))
    (incf (aref pos 0) (* vel dt (cos theta)))
    (incf (aref pos 1) (* vel dt (sin theta)))
    (particle-collide s))
  (with-slots (parts nups genf) s
    (incf nups)
    (let ((addition (funcall genf s dt)))
      (when addition
	(vector-push-extend addition parts)))
    (loop :for p :being :the :elements :of parts :do
       (update p dt))
    (delete-if-not (lambda (p) (slot-value p 'alive)) parts)))
