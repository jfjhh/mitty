;;;;
;;;; SDL2 Test.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defparameter *width* 1024)
(defparameter *height* 1024)

(defparameter *xs* '(0 1/4 1/2 3/4 1))
(defparameter *ys* '(0 1/4 1/2 3/4 1))

(defparameter *time* 0d0)
(defparameter *frame* 0)

(defparameter *bullet* nil)

(defparameter *bscreen*
  (make-instance 'sdl-screen
		 :width *width*
		 :height *height*))

(defun reset ()
  (setf *time* 0d0)
  (setf *bullet*
	(make-instance 'rot-particle
		       :pos (grid:make-foreign-array
			     'double-float
			     :initial-contents '(0d0 0d0))
		       :vel 0.01d0
		       :omega 0d0
		       :theta (+ 0.1 (float (/ pi 2) 0d0)))))

(defun func->bspline (a b n f &optional (nverts (* n 2)))
  (let* ((fstep (/ (- b a) n))
	 (xarr
	  (grid:make-foreign-array
	   'double-float
	   :initial-contents
	   (loop :for i :from a :below b :by fstep :collecting i)))
	 (yarr
	  (grid:make-foreign-array
	   'double-float
	   :initial-contents
	   (loop :for i :from a :below b :by fstep :collecting (funcall f i))))
	 (spline (gsll:make-spline gsll:+cubic-spline-interpolation+ xarr yarr)))
    (let* ((begin (grid:aref xarr 0))
	   (end (grid:aref xarr (1- n)))
	   (vstep (/ (- end begin) nverts)))
      (loop :for xi :from begin :below end :by vstep
	 :collecting (list xi (gsll:evaluate spline xi))))))

(defun draw-verts (xs ys)
  (loop :for x :in xs :for y :in ys :do
     (sdl:draw-filled-circle
      (sdl:point :x  x :y y)
      2
      :color (sdl:color :r 128 :g 0 :b 128)
      :stroke-color (sdl:color :r 0 :g 0 :b 255))))

(defun draw-verts* (ps)
  (loop :for p :in ps :do
     (let ((x (car p))
	   (y (cadr p)))
       (sdl:draw-filled-circle
	(sdl:point :x  x :y y)
	2
	:color (sdl:color :r 128 :g 0 :b 128)
	:stroke-color (sdl:color :r 0 :g 0 :b 255)))))

(defun to-points (xs ys)
  (mapcar (lambda (x y) (sdl:point :x x :y y)) xs ys))

(defun run-sdl-thread (&optional (func #'bezier) (name "Mitty"))
  (bt:make-thread func :name name))

(defmethod bezier ()
  (let ((width *width*)
	(height *height*))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
	  (reset)))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)  
			 (when (some (curry #'sdl:key= key)
				     (list :sdl-key-escape :sdl-key-q))  
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       ;; Run the loop but also keep SLIME working for interactivity.
	       #||
	       (let ((connection
	       (or swank::*emacs-connection* (swank::default-connection))))
	       (when (and connection
	       (not (eql swank:*communication-style* :spawn)))
	       (swank::handle-requests connection t)))
	       ||#

	       ;; Main stuff.
	       ;; Blanking Box
	       ;(sdl:draw-box (sdl:rectangle :x 0 :y 0 :w width :h height) :color (sdl:color :r 0 :g 0 :b 0) :alpha 8)

	       #||
	       ;; Particle stuff.
	       (incf *time* 0.05d0)
	       (setf (slot-value *bullet* 'omega) (expt *time* 2))
	       (update-particle *bullet* 0.1d0)
	       (draw-particle *bullet* *bscreen*)
	       ||#
	       ;(when (zerop (setf *frame* (mod (1+ *frame*) 10))))
	       (sdl:update-display))))))
