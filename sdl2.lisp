;;;;
;;;; SDL2 Test.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter *xs* '(0 1/4 1/2 3/4 1))
(defparameter *ys* '(0 1/4 1/2 3/4 1))

(defparameter *time* 0d0)
(defparameter *frame* 0)

(defparameter *bullet* nil)
(defparameter *sp* nil)

(defparameter *screen*
  (make-instance 'sdl-screen
		 :width 512d0
		 :height 512d0))

(defun clear (&optional (color (sdl:color :r 0 :g 0 :b 0)))
  (sdl:clear-display color))

(defun reset ()
  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
  (sdl:update-display)
  (setf *time* 0d0)
  (setf *sp*
	(make-instance
	 'spell
	 :vel 1d0
	 :omega -1d-2
	 :genf (lambda (s &rest x)
		 (declare (ignore x))
		 (with-slots (nups pos) s
		   (make-instance 'rot-particle
				  :pos (copy pos)
				  :vel 0.0005d0
				  :acc 0d0
				  :theta (* 0.5d0 nups)
				  :omega 0d0)))))
  (setf *bullet*
	(make-instance 'rot-particle
		       :pos (make-foreign-array
			     'double-float
			     :initial-contents '(0d0 0d0))
		       :vel 0.01d0
		       :omega 0d0
		       :theta (+ 0.1d0 (float (/ pi 2d0) 0d0)))))

(defun run-sdl-thread (&optional (func #'mitty-main) (name "Mitty"))
  (bt:make-thread func :name name))

(defun mitty-main ()
  (with-slots (width height) *screen*
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 60)
      (reset)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)  
			 (when (some (curry #'sdl:key= key)
				     (list :sdl-key-escape :sdl-key-q))  
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       #||
	       ;; Particle stuff.
	       (incf *time* 0.01d0)
	       (setf (slot-value *bullet* 'omega)
	       (+ 0d0
	       (* 30d0 (sin (* 3d0 *time*)))
	       (* 50d0 (cos (* 50d0 *time*)))))
	       (update *bullet* 0.1d0)
	       (draw *bullet* *screen* :clear nil)

	       (update *sp* 3d0)
	       (draw *sp* *screen*)
	       ||#

	       (sdl:update-display))))))
