;;;;
;;;; Curve Particles
;;;; Alex Striff
;;;;

(in-package #:mitty)

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
    ;; TODO: Figure out and apply external forces to particle acceleration.
    (antik:incf pos (antik:* vel dt))
    (antik:incf vel (antik:* acc dt))

    ;; Wrap around view box.
    #||
    (setf pos
	  (antik:* pos
		   (map-grid :source pos :element-function
			     (lambda (x) (if (< 320d0 (abs x)) -1d0 1d0)))))
    ||#
    )) 

(defun add-bullet-child (update-func)
  "Makes a bullet update function that adds the result of update-func to (children bullet)."
  (lambda (bullet dt)
    (with-slots (children) bullet
      (when (queue-full-p children)
	(dequeue children))
      (enqueue (funcall update-func bullet dt) children))))

(defgeneric kinetic-energy (physical-object)
  (:documentation "Calculate the kinetic energy associated with physical-object."))

(defmethod kinetic-energy ((particle physical-particle))
  (with-slots (mass vel) particle
    (let ((speed (norm vel)))
      (* 0.5d0 mass (* speed speed)))))

(defgeneric gravitational-force (pobject1 pobject2)
  (:documentation "Calculates the gravitational force of pobject 1 on pobject 2"))

(defgeneric couloumb-force (pobject1 pobject2)
  (:documentation "Calculates the electrical force of pobject 1 on pobject 2"))

(defgeneric net-force (pobject1 pobject2)
  (:documentation "Calculates the net force of pobject 1 on pobject 2"))

(defmethod gravitational-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (let* ((r (antik:- (pos pobject1) (pos pobject2))))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +gravitational-constant+ (mass pobject1) (mass pobject2)) (* |r| |r|))))))

(defmethod couloumb-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (let* ((r (antik:- (pos pobject1) (pos pobject2))))
    (multiple-value-bind (r^ |r|) (normalize r)
      (antik:* r^ (/ (* +couloumb-number+ (charge pobject1) (charge pobject2)) (* |r| |r|))))))

(defmethod net-force ((pobject1 physical-particle) (pobject2 physical-particle))
  (antik:+ (gravitational-force pobject1 pobject2)
	   (couloumb-force pobject1 pobject2)))

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

(defun random-pos-physical-particle (d)
  (let ((p (make-physical-particle 3)))
    (setf (pos p)
	  (apply #'grid
		 (mapcar (lambda (x)
			   (declare (ignore x))
			   (- 320 (random 640)))
			 (iota d))))
    p))

(defun random-pos-physical-particles (n d)
  (let ((particles (make-array n)))
    (dotimes (i n)
      (setf (svref particles i) (random-pos-physical-particle d)))
    particles))
