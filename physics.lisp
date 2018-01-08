;;;;
;;;; Curve Particles
;;;; Alex Striff
;;;;

(in-package #:mitty)

;;; Convenience types and constants.

(deftype rvector (n) `(simple-array double-float (,n)))
(deftype queue () 'simple-vector)

(deftype scalar () 'double-float)
(deftype mass   () '(double-float 0d0 *)) ; Non-negative.
(deftype charge () 'double-float)

(defconstant +earth-g+ 9.81d0)
(defconstant +gravitational-constant+ 6.674d-11)
(defconstant +elementary-charge+ 1.602176620898d-19)
(defconstant +vacuum-permittivity+ 8.854187817d-12)
(defconstant +couloumb-number+ (/ (* 4d0 pi +vacuum-permittivity+)))

;;; Spring simulations are numerically unstable. These values need tuning for
;;; stability, especially *damping*, that depends roughly upon the energy of the
;;; system.
(defparameter *default-mass* 1d1)
(defparameter *default-charge* +elementary-charge+)
(defparameter *default-elasticity* 1d-2)
(defparameter *default-equilibrium* 1d2)
(defparameter *damping* 5d-2)

;;; Physics and bullet objects.

(defclass physical-object ()
  ((mass :initarg :mass
	 :accessor mass
	 :type mass
	 :documentation "The mass of the particle in kilograms.")
   (charge :initarg :charge
	   :accessor charge
	   :type charge
	   :documentation "The charge of the particle in Couloumbs."))
  (:default-initargs :mass 0d0 :charge 0d0)
  (:documentation "A physical object with at least mass and charge."))

(defclass spring (physical-object)
  ((elasticity :initarg :elasticity
	       :accessor elasticity
	       :type scalar
	       :documentation "The linear elasticity of the spring in N/m.")
   (equilibrium :initarg :equilibrium
		:accessor equilibrium
		:type scalar
		:documentation "The equilibrium length of the spring in meters.")
   (start :initarg :start
	  :accessor start
	  :type physical-object
	  :documentation "One end of the spring.")
   (end :initarg :end
	:accessor end
	:type physical-object
	:documentation "One end of the spring."))
  (:default-initargs :elasticity *default-elasticity* :equilibrium *default-equilibrium*)
  (:documentation "A spring connection between two physical-objects."))

(defun make-spring (p1 &optional p2
			 (elasticity *default-elasticity*)
			 (equilibrium *default-equilibrium* ep))
  (if p2
      (progn
	(when ep
	  (setf equilibrium (distance (pos p1) (pos p2))))
	(make-instance 'spring
		       :elasticity elasticity
		       :equilibrium equilibrium
		       :start p1
		       :end p2))
      (make-instance 'spring
		     :elasticity elasticity
		     :equilibrium equilibrium
		     :start p1)))

(defclass kinematic-object ()
  ()
  (:documentation "A kinematic object that can move over time."))

(defgeneric update (kinematic-object dt &key)
  (:documentation "Calculates motion of kinematic-object over time change dt."))

(defclass particle (kinematic-object)
  ((pos :initarg :pos
	:accessor pos
	:type rvector
	:documentation "The position of the particle."))
  (:documentation "A particle kinematic object that can move over time."))

(defclass bullet (particle)
  ((ang-pos :initarg :ang-pos
	    :accessor ang-pos
	    :type scalar
	    :documentation "The angular position of the bullet.")
   (state :initarg :state
	  :accessor state
	  :type alist
	  :documentation "Association list of local bullet state.")
   (update-func :initarg :update-func
		:accessor update-func
		:type function
		:documentation "Function run with bullet and dt to update the bullet."))
  (:default-initargs :pos (grid 0d0 0d0) :ang-pos 0d0 :state nil)
  (:documentation "A 2D particle kinematic object that can accelerate itself."))

(defclass angular-bullet (bullet)
  ((ang-vel :initarg :ang-vel
	    :accessor ang-vel
	    :type scalar
	    :documentation "The angular velocity of the bullet.")
   (ang-acc :initarg :ang-acc
	    :accessor ang-acc
	    :type scalar
	    :documentation "The angular acceleration of the bullet."))
  (:default-initargs :ang-vel 0d0 :ang-acc 0d0)
  (:documentation "A bullet with kinematic angular motion."))

(defclass linear-bullet (bullet)
  ((vel :initarg :vel
	:accessor vel
	:type scalar
	:documentation "The velocity of the bullet.")
   (acc :initarg :acc
	:accessor acc
	:type scalar
	:documentation "The acceleration of the bullet."))
  (:default-initargs :vel 0d0 :acc 0d0)
  (:documentation "A bullet with kinematic linear motion."))

(defclass kinematic-bullet (angular-bullet linear-bullet)
  ()
  (:documentation "A bullet with full kinematic motion."))

(defclass generator-bullet (kinematic-bullet)
  ((children :initarg :children
	     :accessor children
	     :type queue
	     :documentation "The composite children of the bullet."))
  (:documentation "A kinematic bullet that can generate child bullets of its own."))

(defmethod initialize-instance :after ((bullet generator-bullet) &key (max-children 16))
  "Initializes the child queue of generator-bullet bullet to hold max-children."
  (setf (slot-value bullet 'children) (make-queue max-children)))

(defclass physical-particle (physical-object particle)
  ((vel :initarg :vel
	:accessor vel
	:type rvector
	:documentation "The velocity of the particle.")
   (acc :initarg :acc
	:accessor acc
	:type rvector
	:documentation "The acceleration of the particle."))
  (:default-initargs :mass *default-mass* :charge *default-charge*)
  (:documentation "A particle that can experience kinematic motion from forces."))

(defclass massive-particle (physical-particle)
  ()
  (:documentation "A physical-particle that is affected by gravitational fields."))

(defclass charged-particle (physical-particle)
  ()
  (:documentation "A physical-particle that is affected by electric fields."))

(defclass spring-particle (physical-particle)
  ((springs :initarg :springs
	    :accessor springs
	    :type vector
	    :documentation "A collection of springs connected to the particle."))
  (:documentation "A physical-particle that is affected by spring forces."))

(defmethod initialize-instance :after ((instance spring-particle) &key (nsprings 2))
  (unless (slot-boundp instance 'springs)
    (setf (slot-value instance 'springs)
	  (make-array nsprings :adjustable t :fill-pointer 0))))

(defun spring-connect (p1 p2 &key
			       (elasticity *default-elasticity*)
			       (equilibrium *default-equilibrium*))
  (let ((spring (make-spring p1 p2 elasticity equilibrium)))
    (vector-push-extend spring (springs p1))
    (vector-push-extend spring (springs p2))))

(defclass field-particle (massive-particle charged-particle)
  ()
  (:documentation "A physical-particle that is affected by fields."))

(defclass full-particle (massive-particle charged-particle spring-particle)
  ()
  (:documentation "A physical-particle that is affected by everything implemented."))

(defun determine-particle-type (mok cok sok)
  "Determines the type of a physical particle, given the interactions allowed.
   mok t for interaction with gravitational fields.
   cok t for interaction with electric fields.
   sok t for interaction with springs."
  (if sok
      (if (or mok cok) 'full-particle 'spring-particle)
      (if mok
	  (if cok 'field-particle 'massive-particle)
	  (if cok 'charged-particle 'physical-particle))))

(defun make-physical-particle (d &key pos vel acc
				   (mass *default-mass* m*)
				   (charge *default-charge* c*)
				   (mok m*) (cok c*) sok)
  "Makes a physical particle of dimensionality n."
  (when (not (plusp d))
    (error "Dimensionality ~d of a physical-particle must be positive." d))
  (when (> d 3)
    (warn "Creating physical-particle of high dimensionality ~d." d))
  (let ((zero (make-grid `((array ,d) double-float))))
    (make-instance (determine-particle-type mok cok sok)
		   :pos (or pos zero)
		   :vel (or vel zero)
		   :acc (or acc zero)
		   :mass mass
		   :charge charge)))

;;; Update: simulation steps.

(defmethod update :before ((bullet bullet) dt &key)
  "Runs the bullet update-func before doing other kinetic updates."
  (when (slot-boundp bullet 'update-func)
    (funcall (update-func bullet) bullet dt)))

(defmethod update ((bullet angular-bullet) dt &key)
  "Steps bullet angular position and angular velocity by dt."
  (with-slots (ang-pos ang-vel ang-acc) bullet
    (incf ang-pos (* ang-vel dt))
    (incf ang-vel (* ang-acc dt)))
  (call-next-method))

(defmethod update ((bullet linear-bullet) dt &key)
  "Steps bullet position by velocity given by bullet vel in bullet direction."
  (with-slots (pos ang-pos vel acc) bullet
    (incf vel (* acc dt))
    (incf (aref pos 0) (* vel dt (cos ang-pos)))
    (incf (aref pos 1) (* vel dt (sin ang-pos)))))

(defmethod update :after ((bullet generator-bullet) dt &key)
  "Update generator-bullet children with time step dt."
  (with-slots (children) bullet
    (loop :for i :from 2 :to (queue-count children) :do
       (update (svref children i) dt))))

(defmethod update :before ((particle spring-particle) dt &key)
  (with-slots (vel acc) particle
    (antik:decf acc (antik:* *damping* vel))))

(defmethod update ((particle physical-particle) dt &key)
  "Steps particle position and velocity by dt."
  (with-slots (pos vel acc) particle
    (antik:incf pos (antik:* vel dt))
    (antik:incf vel (antik:* acc dt)))) 

;;; Update utility functions.

(defun add-bullet-child (update-func)
  "Makes a bullet update function that adds the result of update-func to (children bullet)."
  (lambda (bullet dt)
    (with-slots (children) bullet
      (when (queue-full-p children)
	(dequeue children))
      (enqueue (funcall update-func bullet dt) children))))

;;; Pairwise or unary physics methods.

(defgeneric kinetic-energy (physical-object)
  (:documentation "Calculate the kinetic energy associated with physical-object."))

(defmethod kinetic-energy ((particle physical-particle))
  (with-slots (mass vel) particle
    (let ((speed (norm vel)))
      (* 0.5d0 mass (* speed speed)))))

(defgeneric gravitational-potential (pobject1 position)
  (:documentation "Calculates the gravitational potential due to pobject1 at position."))

(defmethod gravitational-potential ((pobject1 physical-particle) position)
  (let* ((r (norm (antik:- (pos pobject1) position))))
    (/ (* +gravitational-constant+ (mass pobject1)) r)))

(defgeneric electric-potential (pobject1 position)
  (:documentation "Calculates the electric potential due to pobject1 at position."))

(defmethod electric-potential ((pobject1 physical-particle) position)
  (let* ((r (norm (antik:- position (pos pobject1)))))
    (/ (* +couloumb-number+ (charge pobject1)) r)))

(defgeneric gravitational-field (pobject1 position)
  (:documentation "Calculates the gravitational field due to pobject1 at position."))

(defmethod gravitational-field ((pobject1 physical-particle) position)
  (let* ((r (antik:- (pos pobject1) position)))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +gravitational-constant+ (mass pobject1)) (* |r| |r|))))))

(defgeneric electric-field (pobject1 position)
  (:documentation "Calculates the electric field due to pobject1 at position."))

(defmethod electric-field ((pobject1 physical-particle) position)
  (let* ((r (antik:- position (pos pobject1))))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +couloumb-number+ (charge pobject1)) (* |r| |r|))))))

(defgeneric gravitational-potential-gradient (pobject1 position)
  (:documentation
   "Calculates the gradient of the gravitational potential due to pobject1 at position."))

(defmethod gravitational-potential-gradient ((pobject1 physical-particle) position)
  (antik:- (gravitational-field pobject1 position)))

(defgeneric electric-potential-gradient (pobject1 position)
  (:documentation
   "Calculates the gradient of the electric potential due to pobject1 at position."))

(defmethod electric-potential-gradient ((pobject1 physical-particle) position)
  (antik:- (electric-field pobject1 position)))

(defgeneric gravitational-force (pobject1 pobject2)
  (:documentation "Calculates the gravitational force of pobject 1 on pobject 2"))

(defmethod gravitational-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:* (mass pobject2) (gravitational-field pobject1 (pos pobject2))))

(defgeneric couloumb-force (pobject1 pobject2)
  (:documentation "Calculates the electric force of pobject 1 on pobject 2"))

(defmethod couloumb-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:* (charge pobject2) (electric-field pobject1 (pos pobject2))))

(defun net-spring-force (particle)
  "Calculates the spring force on particle from its springs."
  (with-slots (pos springs) particle
    (let* ((zero (make-grid `((array ,(length pos)) double-float)))
	   (force zero))
      (dotimes (i (length springs))
	(with-slots (elasticity equilibrium start end) (aref springs i)
	  (when (or (eq particle start) (eq particle end))
	    (antik:incf force
			(let ((r (if (eq particle start)
				     (antik:- (pos start) (pos end))
				     (antik:- (pos end) (pos start)))))
			  (multiple-value-bind (r^ |r|) (normalize r)
			    (antik:* (* elasticity (- equilibrium |r|)) r^)))))))
      force)))

(defun g+ (&rest grids)
  "Each of grids is a vector of grids. These are all added componentwise."
  (let* ((g (car grids))
	 (size (length g)) 
	 (out (make-array size
			  :element-type (type-of (aref g 0))
			  :initial-element (grid 0 0 0))))
    (dolist (grid grids)
      (dotimes (i size)
	(antik:incf (aref out i) (aref grid i))))
    out))

(eval-when (:compile-toplevel :load-toplevel)
  (define-method-combination antik:+ :identity-with-one-argument t))

(defgeneric net-force (pobject1 pobject2)
  (:method-combination antik:+ :most-specific-last)
  (:documentation "Calculates the net force of pobject 1 on pobject 2"))

(defmethod net-force antik:+ ((pobject1 physical-particle) (pobject2 massive-particle))
  (print "MASS ")
  (gravitational-force pobject1 pobject2))

(defmethod net-force antik:+ ((pobject1 physical-particle) (pobject2 charged-particle))
  (print "CHARGE ")
  (couloumb-force pobject1 pobject2))

;;; Physics methods on many particles.

(defun all-pairs (list)
  "All distinct pairs of elements of list. For n elements, returns (* 1/2 n (+ n 1)) pairs."
  (mapcon (lambda (x) (mapcar (curry #'cons (car x)) (cdr x))) list))

(defun map-pairs* (function list)
  "Calls function with all distinct pairs of elements in list, collecting (cons pair result)."
  (mapcar (lambda (x) (cons x (funcall function (car x) (cdr x)))) (all-pairs list)))

(defun map-pairs (function list)
  "Calls function with all distinct pairs of elements in list, collecting results."
  (mapcar #'cdr (map-pairs* function list)))

(defun map-vector-pairs* (function vector)
  (map-pairs* (lambda (i j) (funcall function (aref vector i) (aref vector j)))
	      (iota (length vector))))

(defun apply-force (p1 force)
  (antik:incf (acc p1) (antik:* (/ (mass p1)) force)))

(defun apply-force-pair (p1 p2 force-1->2)
  "Changes accelerations of particles p1 and p2 by the internal force-1->2."
  (apply-force p2 force-1->2)
  (apply-force p1 (antik:- force-1->2)))

(defun apply-interaction-forces (psequence)
  "Calculates interaction forces between physical-particles."
  (flet ((pair-force (i j)
	   (let ((p1 (aref psequence i))
		 (p2 (aref psequence j)))
	     (net-force p1 p2)))
	 (apply-force-pair* (x)
	   (destructuring-bind ((i . j) . force) x
	     (let ((p1 (aref psequence i))
		   (p2 (aref psequence j)))
	       (apply-force-pair p1 p2 force)))))
    (let ((n (length psequence)))
      (dotimes (i n)
	(let ((p (aref psequence i)))
	  (setf (acc p) (antik:* (acc p) 0d0))))
      (mapc #'apply-force-pair*
	    (map-pairs* #'pair-force (iota (length psequence)))))))

(defun apply-spring-forces (psequence)
  (let ((spring-particles (remove-if-not (lambda (p) (typep p 'spring-particle))
					 psequence)))
    (dotimes (i (length spring-particles))
      (let ((particle (aref spring-particles i)))
	(setf (acc particle) (grid 0 0 0))
	(apply-force particle (net-spring-force particle))))))

(defun electric-field-line (charge-particles)
  "Returns a function that will calculate the electric field due to
   charge-particles at a point."
  (let ((length (length charge-particles)))
    (lambda (position)
      (let ((field (grid 0 0 0)))
	(dotimes (i length)
	  (antik:incf field
		      (electric-field (aref charge-particles i) position)))
	(normalize field)))))

(defun electric-equipotential-line (charge-particles)
  "Returns a function that will calculate a equipotential vector that is
   perpendicular to the electric field line due to charge-particles at a point."
  (let ((field-line (electric-field-line charge-particles)))
    (lambda (position)
      (let* ((field (funcall field-line position))
	     (x (aref field 0))
	     (y (aref field 1))
	     (z (aref field 2)))
	(grid (- y) x z)))))

(defun do-path-steps (gradient-function initial-position point-function
		      &key
			(step 10d0) (max 1024) (stop-at 'oscillation)
			(period-epsilon (1- step)))
  "Calls point-function at points obtained by stepping initial-position by
   (* gradient-function step), with maximum number of steps max. When stop-at
   is 'oscillation, stepping stops if the inner product of the differences
   between the three latest steps is negative (like (inner (- s1 s2) (- s2 s3)).
   When stop-at is 'period, stop when the position is period-epsilon away from
   initial-position."
  (declare (type function gradient-function point-function)
	   (type double-float step period-epsilon)
	   (type fixnum max))
  (funcall point-function initial-position)
  (let ((position initial-position)
	(old initial-position)
	older)
    (loop :for i :from 1 :below max :do
       (setf older old old position)
       ;; Quick low-order approximation, but still better than Euler.
       (let* ((guess (funcall gradient-function position))
	      (euler (funcall gradient-function
			      (antik:+ position (antik:* step guess)))))
	 (antik:incf position
		     (antik:* (/ step 2) (antik:+ guess euler)))
	 (funcall point-function position)
	 (when
	     (or (< 320 (abs (aref position 0)))
		 (< 320 (abs (aref position 1)))
		 (case stop-at
		   (oscillation
		    (minusp (the double-float
				 (grid:inner (antik:- old older)
					     (antik:- position old)))))
		   (period
		    (< (the double-float (distance initial-position position))
		       period-epsilon))))
	   (loop-finish))))))

(defun transpose (2d-grid)
  "Transposes the grid of grids 2d-grid into a real multidimensional array."
  (let* ((points (length 2d-grid))
	 (components (length (aref 2d-grid 0)))
	 (coordinates (make-array `(,components ,points) :element-type
				  `(simple-array double-float (,components ,points)))))
    (dotimes (j points)
      (dotimes (i components)
	(setf (aref coordinates i j)
	      (aref (aref 2d-grid j) i))))
    coordinates))

(defun draw-connected-points (&optional (screen *screen*)
				(color (rainbow-color (setf *cparam* (mod (+ *cparam* 1.5d-3) 1d0)))))
  "Returns a function that when called successively with points, will draw
   those points connected together to screen with color color."
  (let (old)
    (lambda (point)
      (let ((position (project-point point screen)))
	(when old
	  (let ((px (aref position 0))
		(py (aref position 1))
		(ox (aref old 0))
		(oy (aref old 1)))
	    (draw-out-line ox oy px py color)))
	(setf old position)))))

(defun electric-field-line-curve (charges start
				  &key (type 'cspline) equipotential (step 10d0)
				    (max 1024) (invert nil) (draw t) (interpolate nil))
  "Draws or interpolates an electric field or equipotential curve for charges from start.
   :type is an interpolation type.
   :equipotential t draws an equipotential curve, otherwise an electric field curve.
   :step is the path step size.
   :max is the maximum number of steps.
   :invert t uses a negative test charge, otherwise positive.
   :draw t draws the curve to *screen*.
   :interpolate t will return the interpolation of the curve with :type."
  (let ((points (when interpolate (make-array 1024 :adjustable t :fill-pointer 0))))
    (flet ((build-gradient-function ()
	     (let ((field-function (the function
					(if equipotential
					    (electric-equipotential-line charges)
					    (electric-field-line charges)))))
	       (if invert
		   (lambda (x) (antik:- (funcall field-function x)))
		   field-function)))
	   (build-point-function ()
	     (let ((draw-function (the function (draw-connected-points)))
		   (interpolate-function (lambda (x) (vector-push-extend x points))))
	       (if draw
		   (if interpolate
		       (lambda (x)
			 (funcall draw-function x)
			 (funcall interpolate-function x))
		       draw-function)
		   (if interpolate
		       interpolate-function
		       (error "No electric-field-line-curve function specified!"))))))
      (let ((gradient-function (build-gradient-function))
	    (point-function (build-point-function)))
	(do-path-steps gradient-function start point-function
		       :stop-at (if equipotential 'period 'oscillation)
		       :step step
		       :max max)
	(when interpolate
	  (let* ((coordinates (transpose points))
		 (interpolations (interpolate coordinates type)))
	    (make-plane-curve (aref interpolations 0)
			      (aref interpolations 1))))))))

(defun circumscribe (n &optional (offset 0d0))
  "Returns n uniform points on the unit circle, from (1, 0), rotated by offset."
  (flet ((circle-point (i)
	   (let ((angle (+ offset (* tau (/ i n)))))
	     (grid (cos angle)
		   (sin angle)
		   0d0))))
    (map 'vector #'circle-point (iota n))))

(defun points-from (point n &optional (direction (grid 1d0 0d0 0d0)) (offset 1))
  "Returns n points on a line from point, stepping by direction, from offset steps."
  (map 'vector (lambda (i) (antik:+ point (antik:* (+ i offset) direction))) (iota n)))

(defun points-around (point n &optional (r 1d0) (offset 0d0))
  "Returns n points in a circle with radius r around point rotated by offset."
  (map 'vector (lambda (x) (antik:+ point (antik:* r x))) (circumscribe n offset)))

(defun electric-field-line-curves (charges n r
				   &key (type 'cspline) equipotential (step 15d0) (max 128) (clear t)
				     (draw t) (interpolate nil))
  "Draws or interpolates n electric field or equipotential curves for charges from start.
   n curves will be calculated at radius r from each of the charges.
   When :equipotential t, step by r between curves.
   Other keywords are for electric-field-line-curve."
  (when clear
    (clear (sdl:color)))
  (let ((source-charges charges))
    (prog1
	(loop :for i :below (length source-charges) :collecting
	   (with-slots (pos charge) (aref source-charges i)
	     (let ((regions (if equipotential
				(points-from pos n
					     (antik:* r (normalize (grid 1 (sqrt 2) 0))) 3)
				(points-around pos n r))))
	       (loop :for start :being :the :elements :of regions :collecting
		  (progn
		    (electric-field-line-curve charges start
					       :type type
					       :equipotential equipotential
					       :step step
					       :max max
					       :invert (minusp charge)
					       :draw draw
					       :interpolate interpolate))))))
      (dotimes (i (length charges))
	(draw (aref charges i) *screen* :size 5d0)))))

(defun simulation-step (particles &optional (dt 1d0) (draw t) (draw-field nil))
  "Do a simulation step of particles with time step dt.
   :draw t will draw the particles to *screen*.
   :draw-field t (requiring :draw t) will draw electric field line curves."
  ;; For spring demo.
  ;(apply-interaction-forces particles)
  (apply-spring-forces particles)
  (dotimes (i (length particles))
    (update (aref particles i) dt))
  (when draw
    (if draw-field
	(electric-field-line-curves particles 8 1d0 :max 48 :step 15d0)
	(progn
	  (clear)
	  (let* ((positions (map-grid :source particles :element-function #'pos))
		 (positions* (concatenate 'vector positions (vector (aref positions 0))))
		 (coordinates (transpose positions*))
		 (interpolates (interpolate coordinates 'periodic-cspline))
		 (plane-curve (make-plane-curve (aref interpolates 0)
						(aref interpolates 1))))
	    (draw plane-curve *screen*
		  :clear nil :normals nil :base nil :n (* 3 (length positions))))
	  (dotimes (i (length particles))
	    (draw (aref particles i) *screen* :size 5d0))))))
