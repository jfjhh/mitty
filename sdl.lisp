;;;;
;;;; SDL Main.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter *deform* nil
  "The curve that is deformed and displayed in the window.")
(defparameter *deform-curve* nil
  "Boolean to deform the curve *deform* or not.")

(defparameter *screen*
  (make-instance 'sdl-screen
		 :width 640d0
		 :height 640d0)
  "The main SDL screen.")

(defun clear (&optional (color (sdl:color :r 0 :g 0 :b 0)))
  "Clears the SDL screen with color, default black."
  (sdl:clear-display color))

(defun reset ()
  "Resets the application state to initial values."
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
  "Convenience to run the SDL thread so that the REPL does not block."
  (bt:make-thread func :name name))

(defun mitty-main ()
  "Main SDL function for Mitty."
  (with-slots (width height) *screen*
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 12) ; Currently controls rate of deformation.
      (reset)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)  
			 ;; Quit with ESC or q.
			 (when (some (curry #'sdl:key= key)
				     (list :sdl-key-escape :sdl-key-q))  
			   (sdl:push-quit-event))

			 ;; Reset and deform from beginning with r.
			 (when (sdl:key= key :sdl-key-r)
			   (reset)
			   (draw *deform-curve* *screen* :n (* 3 (size *deform-curve*))
				 :clear nil :base t :normals t))

			 ;; Toggle deformation evolution on or off with d.
			 (when (sdl:key= key :sdl-key-d)
			   (setf *deform* (not *deform*))))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       ;; Deform the curve.
	       (when *deform*
		 (clear)
		 (setf *cparam* (mod (+ *cparam* 0.0025d0) 1d0))
		 (draw-deform *deform-curve* #'minimize-curvature 8))

	       (sdl:update-display))))))
