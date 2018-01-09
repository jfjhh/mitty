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

(defparameter *simulate* t
  "Boolean to simulate physics of *simulation-particles* or not.")
(defparameter *simulation-particles* nil
  "Collection of particles to simulate")
(defparameter *simulation-dt* 5d-1
  "Time step for the simulation")

(defparameter *danmaku* t
  "Boolean to run danmaku updates or not.")
(defparameter *danmaku-dt* 1d0
  "Time step for bullet kinetics.")
(defparameter *danmaku-generator* nil
  "The parent bullet for the danmaku.")

(defparameter *screen*
  (make-instance 'sdl-screen
		 :width 1024d0
		 :height 1024d0)
  "The main SDL screen.")

(defun clear (&optional (color (sdl:color :r 0 :g 0 :b 0)))
  "Clears the SDL screen with color, default black."
  (sdl:clear-display color))

(defun reset ()
  "Resets the application state to initial values."
  (clear)
  (sdl:update-display)

  ;; Reset curve deforming.
  (let ((abscissa (rlambda (x) (+ (cos x) (* -1/3 (cos (* x 3))))))
	(ordinate (rlambda (x) (+ (sin x) (*  1/8 (sin (* x 7)))))))
    (setf (domain abscissa) (interval 0 tau))
    (setf (domain ordinate) (interval 0 tau))
    (setf *deform* nil)
    (setf *deform-curve*
	  (make-plane-curve
	   (interpolate abscissa 'periodic-cspline :n 32)
	   (interpolate ordinate 'periodic-cspline :n 32))))

  ;; Reset simulation.
  (let* ((n 24)
	 (r 160d0)
	 (particles (map 'vector
			 (lambda (x) (make-physical-particle
				 3
				 :pos x
				 :vel (antik:* 2d-1 (abs (sin (/ (aref x 0) r)))
					       (grid (+ (aref x 0) (* 1d0 (aref x 1)))
						     (* 1d0 (aref x 0))
						     0))
				 :sok t))
			 (points-around (grid 0 0 -320) n r))))
    (dotimes (i n)
      (dotimes (j n)
	(unless (= i j)
	  (spring-connect (aref particles i) (aref particles j)))))
    (setf *simulate* nil)
    (setf *simulation-particles* particles))

  (setf *danmaku* nil)
  (setf *danmaku-dt* 1d0)
  (setf *danmaku-generator*
	(make-instance
	 'generator-bullet
	 :vel 5d-1
	 :max-children 512
	 :update-func (add-bullet-child
		       (let ((angle 0))
			 (lambda (bullet dt)
			   (declare (ignore dt))
			   (make-instance
			    'kinematic-bullet
			    :pos (copy (pos bullet))
			    :vel 1d0
			    :ang-pos (incf angle (/ (1+ (sqrt 5)) 2)))))))))

(defun run-sdl-thread (&optional (func #'mitty-main) (name "Mitty"))
  "Convenience to run the SDL thread so that the REPL does not block."
  (bt:make-thread func :name name))

(defun mitty-main ()
  "Main SDL function for Mitty."
  (with-slots (width height) *screen*
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 30) ; Currently controls rate of deformation.
      ;(setf (sdl:frame-rate) 12) ; Currently controls rate of deformation.
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
			   (setf *wrinkle* (not *wrinkle*)))

			 ;; Toggle simulation on or off with s.
			 (when (sdl:key= key :sdl-key-s)
			   (setf *simulate* (not *simulate*)))

			 ;; Toggle danmaku on or off with b.
			 (when (sdl:key= key :sdl-key-b)
			   (setf *danmaku* (not *danmaku*))))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (when (or *deform* *simulate* *danmaku*)
		 (clear)

		 ;; Deform the curve.
		 (when *deform*
		   (setf *cparam* (mod (+ *cparam* 1d-4) 1d0))
		   (draw-deform *deform-curve* (if *wrinkle* #'wrinkle #'circulate) 8))

		 ;; Simulate particle motion.
		 (when *simulate*
		   (simulation-step *simulation-particles* *simulation-dt* t nil))

		 ;; Update Danmaku kinetics.
		 (when *danmaku*
		   (update *danmaku-generator* *danmaku-dt*)
		   (draw *danmaku-generator* *screen*))

		 (sdl:update-display)))))))
