;;;;
;;;; SDL2 Test.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize speed))

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
	 :vel 1e-3
	 :omega -1e-2
	 :genf (lambda (s &rest x)
		 (declare (ignore x))
		 (with-slots (nups pos) s
		   (make-instance 'rot-particle
				  :pos (grid:copy pos)
				  :vel 0.0005d0
				  :acc 0d0
				  :theta (* 0.5 nups)
				  :omega 0d0)))))
  (setf *bullet*
	(make-instance 'rot-particle
		       :pos (grid:make-foreign-array
			     'double-float
			     :initial-contents '(0d0 0d0))
		       :vel 0.01d0
		       :omega 0d0
		       :theta (+ 0.1 (float (/ pi 2) 0d0)))))

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
		     (+ 0
			(* 3 (sin (* 3 *time*)))
			(* 5 (cos (* 50 *time*)))))
	       (update *bullet* 0.01d0)
	       (draw *bullet* *screen*)
	       ||#

	       #||
	       (update *sp* 3d0)
	       (draw *sp* *screen*)
	       (setf *cparam* (mod (+ *cparam* 0.01d0) 1d0))
	       ||#

	       (sdl:update-display))))))
