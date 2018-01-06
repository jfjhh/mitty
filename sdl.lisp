;;;;
;;;; SDL Main.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defparameter *deform* nil
  "The curve that is deformed and displayed in the window.")
(defparameter *wrinkle* nil
  "Wrinkle *deform-curve* if t, otherwise circulate.")
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
  (let ((abscissa (rlambda (x) (+ (cos x) (* -1/3 (cos (* x 3))))))
	(ordinate (rlambda (x) (+ (sin x) (*  1/8 (sin (* x 7)))))))
    (setf (domain abscissa) (interval 0 tau))
    (setf (domain ordinate) (interval 0 tau))
    (setf *deform* nil)
    (setf *deform-curve*
	  (make-plane-curve
	   (interpolate abscissa 'periodic-cspline :n 32)
	   (interpolate ordinate 'periodic-cspline :n 32)))))

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
			   (setf *deform* (not *deform*)))

			 ;; Toggle wrinkling on or off with w.
			 (when (sdl:key= key :sdl-key-w)
			   (setf *wrinkle* (not *wrinkle*))))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       ;; Deform the curve.
	       (when *deform*
		 (clear)
		 (setf *cparam* (mod (+ *cparam* 1d-4) 1d0))
		 (draw-deform *deform-curve* (if *wrinkle* #'wrinkle #'circulate) 8))

	       (sdl:update-display))))))
