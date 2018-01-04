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
    (antik:incf vel (antik:* acc dt)))) 

(defun add-bullet-child (update-func)
  (lambda (bullet dt)
    (with-slots (children) bullet
      (when (queue-full-p children)
	(dequeue children))
      (enqueue (funcall update-func bullet dt) children))))
