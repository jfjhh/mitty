;;;;
;;;; SDL2 Test.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter *deform* nil)
(defparameter *deform-curve* nil)

(defparameter *screen*
  (make-instance 'sdl-screen
		 :width 1024d0
		 :height 1024d0))

(defun clear (&optional (color (sdl:color :r 0 :g 0 :b 0)))
  (sdl:clear-display color))

(defun reset ()
  (clear)
  (sdl:update-display)
  (let ((abscissa (rlambda (x) x))
	(ordinate (rlambda (x) (sin x))))
    (setf (domain abscissa) (interval 0 tau))
    (setf (domain ordinate) (interval 0 tau))
    (setf *deform-curve*
	  (make-plane-curve
	   (interpolate abscissa 'cspline :n 32)
	   (interpolate ordinate 'cspline :n 32)))))

(defun run-sdl-thread (&optional (func #'mitty-main) (name "Mitty"))
  (bt:make-thread func :name name))

(defun mitty-main ()
  (with-slots (width height) *screen*
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 12)
      (reset)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)  
			 (when (some (curry #'sdl:key= key)
				     (list :sdl-key-escape :sdl-key-q))  
			   (sdl:push-quit-event))
			 (when (sdl:key= key :sdl-key-r)
			   (reset)
			   (draw *deform-curve* *screen* :n (* 8 (size *deform-curve*))
				 :clear nil :base t :normals t))
			 (when (sdl:key= key :sdl-key-d)
			   (setf *deform* (not *deform*))))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       ;; Deform the curve.
	       (when *deform*
		 (clear)
		 (setf *cparam* (mod (+ *cparam* 0.0025d0) 1d0))
		 (draw-deform *deform-curve* #'minimize-curvature))

	       (sdl:update-display))))))
