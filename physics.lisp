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
  (:default-initargs :mass 1d0 :charge +elementary-charge+)
  (:documentation "A particle with mass and charge."))

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
  (:documentation "A particle that can experience kinematic motion from forces."))

(defun make-physical-particle (n)
  "Makes a physical particle of dimensionality n."
  (let ((zero (make-grid `((array ,n) double-float))))
    (make-instance 'physical-particle
		   :pos zero
		   :vel zero
		   :acc zero)))

;;; Update: simulation steps.

(defmethod update :before ((bullet bullet) dt &key)
  (when (slot-boundp bullet 'update-func)
    (funcall (update-func bullet) bullet dt)))

(defmethod update ((bullet angular-bullet) dt &key)
  (with-slots (ang-pos ang-vel ang-acc) bullet
    (incf ang-pos (* ang-vel dt))
    (incf ang-vel (* ang-acc dt)))
  (call-next-method))

(defmethod update ((bullet linear-bullet) dt &key)
  (with-slots (pos ang-pos vel acc) bullet
    (incf vel (* acc dt))
    (incf (aref pos 0) (* vel dt (cos ang-pos)))
    (incf (aref pos 1) (* vel dt (sin ang-pos)))))

(defmethod update :after ((bullet generator-bullet) dt &key)
  (with-slots (children) bullet
    (loop :for i :from 2 :to (queue-count children) :do
       (update (svref children i) dt))))

(defmethod update ((particle physical-particle) dt &key)
  (with-slots (pos vel acc) particle
    (antik:incf pos (antik:* vel dt))
    (antik:incf vel (antik:* acc dt)))) 

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

(defgeneric electric-potential (pobject1 position)
  (:documentation "Calculates the electric potential due to pobject1 at position."))

(defgeneric gravitational-field (pobject1 position)
  (:documentation "Calculates the gravitational field due to pobject1 at position."))

(defgeneric electric-field (pobject1 position)
  (:documentation "Calculates the electric field due to pobject1 at position."))

(defgeneric gravitational-potential-gradient (pobject1 position)
  (:documentation
   "Calculates the gradient of the gravitational potential due to pobject1 at position."))

(defgeneric electric-potential-gradient (pobject1 position)
  (:documentation
   "Calculates the gradient of the electric potential due to pobject1 at position."))

(defgeneric gravitational-force (pobject1 pobject2)
  (:documentation "Calculates the gravitational force of pobject 1 on pobject 2"))

(defgeneric couloumb-force (pobject1 pobject2)
  (:documentation "Calculates the electric force of pobject 1 on pobject 2"))

(defgeneric net-force (pobject1 pobject2)
  (:documentation "Calculates the net force of pobject 1 on pobject 2"))

(defmethod gravitational-potential ((pobject1 physical-particle) position)
  (let* ((r (norm (antik:- (pos pobject1) position))))
    (/ (* +gravitational-constant+ (mass pobject1)) r)))

(defmethod electric-potential ((pobject1 physical-particle) position)
  (let* ((r (norm (antik:- position (pos pobject1)))))
    (/ (* +couloumb-number+ (charge pobject1)) r)))

(defmethod gravitational-field ((pobject1 physical-particle) position)
  (let* ((r (antik:- (pos pobject1) position)))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +gravitational-constant+ (mass pobject1)) (* |r| |r|))))))

(defmethod electric-field ((pobject1 physical-particle) position)
  (let* ((r (antik:- position (pos pobject1))))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +couloumb-number+ (charge pobject1)) (* |r| |r|))))))

(defmethod gravitational-potential-gradient ((pobject1 physical-particle) position)
  (antik:- (gravitational-field pobject1 position)))

(defmethod electric-potential-gradient ((pobject1 physical-particle) position)
  (antik:- (electric-field pobject1 position)))

(defmethod gravitational-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:* (mass pobject2) (gravitational-field pobject1 (pos pobject2))))

(defmethod couloumb-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:* (charge pobject2) (electric-field pobject1 (pos pobject2))))

(defmethod net-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:+ (gravitational-force pobject1 pobject2)
	   (couloumb-force pobject1 pobject2)))

;;; Physics methods on many particles.

(defun all-pairs (list)
  (mapcon (lambda (x) (mapcar (curry #'cons (car x)) (cdr x))) list))

(defun map-pairs* (function list)
  (mapcar (lambda (x) (cons x (funcall function (car x) (cdr x)))) (all-pairs list)))

(defun map-pairs (function list)
  (mapcar #'cdr (map-pairs* function list)))

(defun apply-force (p1 p2 force-1->2)
  (antik:incf (acc p2) (antik:/ force-1->2 (mass p2)))
  (antik:incf (acc p1) (antik:/ (antik:- force-1->2) (mass p1))))

(defun apply-interaction-forces (psequence)
  "Calculates interaction forces between physical-particles."
  (flet ((pair-force (i j)
	   (let ((p1 (aref psequence i))
		 (p2 (aref psequence j)))
	     (net-force p1 p2)))
	 (apply-force-pair (x)
	   (destructuring-bind ((i . j) . force) x
	     (let ((p1 (aref psequence i))
		   (p2 (aref psequence j)))
	       (apply-force p1 p2 force)))))
    (let ((n (length psequence)))
      (dotimes (i n)
	(let ((p (aref psequence i)))
	  (setf (acc p) (antik:* (acc p) 0d0))))
      (mapc #'apply-force-pair
	    (map-pairs* #'pair-force (iota (length psequence)))))))

(defun electric-field-line (charge-particles)
  (let ((length (length charge-particles)))
    (lambda (position)
      (let ((field (grid 0 0 0)))
	(dotimes (i length)
	  (antik:incf field
		      (electric-field (aref charge-particles i) position)))
	(normalize field)))))

(defun electric-equipotential-line (charge-particles)
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
		       period-epsilon))
		   (t (error "Stop-at method ~a is not one of oscillation or period."
			     stop-at))))
	   (loop-finish))))))

(defun transpose (2d-grid)
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
				  &key (type 'cspline) equipotentialp (step 10d0)
				    (max 1024) (invert nil) (draw t) (interpolate nil))
  (let ((points (when interpolate (make-array 1024 :adjustable t :fill-pointer 0))))
    (flet ((build-gradient-function ()
	     (let ((field-function (the function
					(if equipotentialp
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
		       :stop-at (if equipotentialp 'period 'oscillation)
		       :step step
		       :max max)
	(when interpolate
	  (let* ((coordinates (transpose points))
		 (interpolations (interpolate coordinates type)))
	    (make-plane-curve (aref interpolations 0)
			      (aref interpolations 1))))))))

(defun circumscribe (n &optional (offset 0d0))
  (flet ((circle-point (i)
	   (let ((angle (+ offset (* tau (/ i n)))))
	     (grid (cos angle)
		   (sin angle)
		   0d0))))
    (map 'vector #'circle-point (iota n))))

(defun points-from (point n &optional (direction (grid 1d0 0d0 0d0)) (offset 1))
  (map 'vector (lambda (i) (antik:+ point (antik:* (+ i offset) direction))) (iota n)))

(defun points-around (point n &optional (r 1d0) (offset 0d0))
  (map 'vector (lambda (x) (antik:+ point (antik:* r x))) (circumscribe n offset)))

(defun electric-field-line-curves (charges n r
				   &key (type 'cspline) equipotentialp (step 15d0) (max 128) (clear t)
				     (draw t) (interpolate nil))
  (when clear
    (clear (sdl:color)))
  (let ((source-charges charges))
    (prog1
	(loop :for i :below (length source-charges) :collecting
	   (with-slots (pos charge) (aref source-charges i)
	     (let ((regions (if equipotentialp
				(points-from pos n
					     (antik:* r (normalize (grid 1 (sqrt 2) 0))) 3)
				(points-around pos n r))))
	       (loop :for start :being :the :elements :of regions :collecting
		  (progn
		    (electric-field-line-curve charges start
					       :type type
					       :equipotentialp equipotentialp
					       :step step
					       :max max
					       :invert (minusp charge)
					       :draw draw
					       :interpolate interpolate))))))
      (dotimes (i (length charges))
	(draw (aref charges i) *screen* :size 5d0)))))

(defun simulation-step (charges &optional (dt 1d0) (draw-field nil) (draw t))
  (apply-interaction-forces charges)
  (dotimes (i (length charges))
    (update (aref charges i) dt))
  (when draw
    (if draw-field
	(electric-field-line-curves charges 8 1d0 :max 48 :step 15d0)
	(progn
	  (clear)
	  (dotimes (i (length charges))
	    (draw (aref charges i) *screen* :size 5d0))))))
