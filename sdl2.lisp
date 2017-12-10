;;;;
;;;; SDL2 Test.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defparameter *xs* '(0 1/4 1/2 3/4 1))
(defparameter *ys* '(0 1/4 1/2 3/4 1))

(defparameter *px* 5/10)
(defparameter *py* 5/10)
(defparameter *vx* 1/100)
(defparameter *vy* 1/100)

(defun draw-verts (xs ys)
  (loop :for x :in xs :for y :in ys :do
     (sdl:draw-filled-circle
      (sdl:point :x  x :y y)
      2
      :color (sdl:color :r 128 :g 0 :b 128)
      :stroke-color (sdl:color :r 0 :g 0 :b 255))))

(defun update-particle (dt vx vy)
  (let ((x (+ *px* (* dt vx) (* 0.1 (- 0.5 (random 1.0)))))
	(y (+ *py* (* dt vy) (* 0.1 (- 0.5 (random 1.0))))))
    (setf vx (- vx))
    (setf vy (- vy))
    (setf *px*
	  (cond ((< x 0) 0)
		((> x 1) 1)
		(t (setf vx (- vx)) x)))
    (setf *py*
	  (cond ((< y 0) 0)
		((> y 1) 1)
		(t (setf vy (- vy)) y)))
    (values vx vy)))

(defun draw-particle (width height)
  (let ((hr 1))
    (sdl:draw-filled-circle
     (sdl:point :x (- (* *px* width) hr)
		:y (- (* *py* height) hr))
     (* hr 2)
     :color (sdl:color :r (- 255 (random 128)) 
		       :g (- 255 (random 128))
		       :b (- 255 (random 128))))))

(defun to-points (xs ys)
  (mapcar (lambda (x y) (sdl:point :x x :y y)) xs ys))

(defun bezier ()
  (let ((width 512)
	(height 512))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mitty | SDL2 Test")
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	  (loop :for i :from 0 :to (/ width 2) :by (/ width 20) :do
	     (sdl:draw-bezier
	      (list (sdl:point :x (- (* 9/20 width) (/ i 2.0))
			       :y (+ 20 i))
		    (sdl:point :x (*  21/20 width)
			       :y (* 1/20 height))
		    (sdl:point :x (* 11/10 width)
			       :y (* 3/4 height))
		    (sdl:point :x (- (* 3/5 width) (/ i 8.0))
			       :y (+ (* 3/4 height) (/ i 4.0))))
	      :segments 64))
	  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
	  ))

      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)  
			 (when (some (curry #'sdl:key= key)
				     (list :sdl-key-escape :sdl-key-q))  
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (sdl:draw-box (sdl:rectangle :x 0 :y 0 :w width :h height)
			     :color (sdl:color :r 0 :g 0 :b 0) :alpha 8)
	       (let* ((xs (loop :repeat 4 :collecting (random 1.0)))
		      (ys (loop :repeat 4 :collecting (random 1.0)))
		      (vxs (mapcar (curry #'* width) xs))
		      (vys (mapcar (curry #'* height) ys)))
		 #||
		 (sdl:draw-bezier (to-points vxs vys)
				  :color (sdl:color :r (random 256)
						    :g (random 256)
						    :b (random 256)))
		 ||#
		 ;(draw-verts vxs vys)
		 (multiple-value-bind (vx* vy*) (update-particle 1 *vx* *vy*)
		   (setf *vx* vx*)
		   (setf *vy* vy*))
		 (draw-particle width height)
		 (sdl:update-display)))))))
