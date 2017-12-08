;;;;
;;;; Base definitions and functions.
;;;; Alex Striff.
;;;;

;;; Idea: Symbolic functions. `funcallable` things like (lambda (x) (* x 2)),
;;; but with the symbolic data inside for things like symbolic differentiaion.
;;; See: <https://www.youtube.com/watch?v=nTI_d-jS6dI>.

;;; Idea: Make vector arithmetic generic, so (+ v1 v2) works.

(in-package #:mitty)

(defconstant tau (* 2 pi))

(defparameter *width* 512)
(defparameter *height* 512)

(defparameter +cont-eps+ (expt 10 -6))

(defun multi-lerp (v &rest stops)
  (let ((len (length stops)))
    (cond ((>= 1 len) (error "invalid number of arguments to multi-lerp: ~a" len))
	  ((= 2 len) (lerp v (car stops) (cadr stops)))
	  (t (multiple-value-bind (a u) (floor (* v (1- len)))
		  (let ((place (nthcdr a stops)))
		    (lerp u (car place) (cadr place))))))))

(defun ccons (cx)
  (cons (realpart cx) (imagpart cx)))

(defun make-verts (f start end n)
  (loop :for x :from start :to end :by (/ (- end start) (1- n))
     :collecting (cons x (funcall f x))))

(defun make-bullet (f x0 y0 v0 n)
  (let ((old (cons x0 y0))
	(step (/ (1- n))))
    (loop :for i :from 0 :to 1 :by step :collecting
       (let* ((dv (* v0 step (funcall f i)))
	      (dx (realpart dv))
	      (dy (imagpart dv))
	      (x (car old))
	      (y (cdr old)))
	 (setf old (cons (+ x dx) (+ y dy)))))))

(defun cxslope (x)
  (let ((a (realpart x))
	(b (imagpart x)))
    (unless (= a 0)
      (/ b a))))

(defun slope (x1 y1 x2 y2)
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
    (unless (< (abs dx) +cont-eps+)
	(/ dy dx))))

(defun slope* (p1 p2)
  (slope (car p1) (cdr p1) (car p2) (cdr p2)))

(defun deriv-verts (verts)
  (let* ((first (car verts))
	 (second (cadr verts)))
    (append 
     (cons nil
	   (loop :for v :in (cddr verts) :collecting
	      (let ((x1 (car first))
		    (y1 (cdr first))
		    (x3 (car v))
		    (y3 (cdr v)))
		(setf second first)
		(setf first v)
		(slope x1 y1 x3 y3)))
	   )
     (list nil))))

(defun make-bullet* (f x0 y0 v0 n)
  (let* ((old (cons x0 y0))
	 (oldv (cxslope v0))
	 (step (/ (1- n))))
    (loop :for i :from 0 :to 1 :by step :collecting
       (let* ((dv (* v0 step (funcall f i)))
	      (dx (realpart dv))
	      (dy (imagpart dv))
	      (x  (car old))
	      (y  (cdr old))
	      (xn (+ x dx))
	      (yn (+ y dy))
	      (m (cxslope dv))
	      (oldm (cxslope oldv)))
	 (setf oldv dv)
	 (setf old (cons xn yn))
	 old))))

(defun draw-verts (verts)
  (set-rgba-fill 1 1 1 1)
  (set-rgba-stroke 1 1 1 1)
  (set-line-width 1/2)
  (let ((n (length verts)))
    (loop :for v :in verts :for i :from 0 :do
       (let ((x (car v))
	     (y (cdr v))
	     (pos (/ i n)))
	 (set-rgba-fill (multi-lerp pos 0 1)
			(multi-lerp pos 0 1 0)
			(multi-lerp pos 1 0)
			1)
	 (centered-circle-path x y 0.75)
	 (fill-path)))))

(defun draw-curve (verts)
  (set-line-width 1/2)
  (let ((len (length verts))
	(x0 0)
	(y0 0))
    (set-rgba-stroke 1 1 1 1)
    (move-to 0 0)
    (dotimes (i len)
      (let* ((v (elt verts i))
	     (p (car v))
	     (c (cdr v))
	     (pos (/ i len))
	     (x (car p))
	     (y (cdr p))
	     (cx (car c))
	     (cy (cdr c)))
	(set-rgba-fill 1/2 1/2 1/2 1)
	(centered-circle-path cx cy 2)
	(fill-path)
	(set-rgba-fill (multi-lerp pos 0 1)
		       (multi-lerp pos 0 1 0)
		       (multi-lerp pos 1 0)
		       1)
	(centered-circle-path x y 3)
	(fill-path)
	#||
	;(move-to x0 y0)
	;(line-to cx cy)
	;(line-to x y)
	(quadratic-to cx cy x y)
	(setf x0 x)
	(setf y0 y)
	||#
	)
      (stroke)
      )))

(defmacro polynomial (var &rest coeffs)
  (let ((deg -1)
	(varsym (gensym)))
    `(let ((,varsym ,var))
       (+ ,@(mapcar (lambda (c) (incf deg) `(* ,c (expt ,varsym ,deg)))
		    (nreverse coeffs))))))

(defun intersect-point (px1 py1 d1 px2 py2 d2)
  (let* ((mx  (/ (+ px1 px2) 2))
	 (my  (/ (+ py1 py2) 2))
	 (mid (or (not d1)
		  (not d2)
		  (< (abs (- d2 d1)) +cont-eps+)))
	 (a   (when d1 (* d1 px1)))
	 (cx  (if mid mx (/ (- (+ py2 a) py1 (* d2 px2)) (- d1 d2))))
	 (cy  (if mid my (- (+ py1 (* d1 cx)) a)))
	 ;;(cx  (if mid 0 (/ (- (+ py2 a) py1 (* d2 px2)) (- d1 d2))))
	 ;;(cy  (if mid 0 (- (+ py1 (* d1 cx)) a)))
	 )
    (cons cx cy)))

(defun test-curves (file)
  (with-canvas (:width *width* :height *height*)
    (let ((hw (/ *width* 2))
	  (hh (/ *height* 2)))
      (declare (type fixnum hw hh))
      (translate hw hh)
      (set-rgba-stroke 1 1 1 0.187)
      (move-to 0 hh)
      (line-to 0 (- hh))
      (stroke)
      (move-to (- hw) 0)
      (line-to hw 0)
      (stroke)
      #||
      (set-rgba-fill 1 1 1 0.75)
      (set-rgba-stroke 1 1 0 0.75)
      (move-to (* -1 hw) (* -1/2 hh))
      (quadratic-to (* -1/2 hw) 0 0 (* -1/2 hh))
      (quadratic-to (* 1/2 hw) (* -1 hh) (* 1 hw) (* -1/2 hh))
      (stroke)
      ||#
      ; (draw-curve
      (draw-verts
	(let ((xoff 0f0)
	      (xmul (* 300))
	      (foff 0f0)
	      (fmul 35f0))
	  (make-bullet*
	   ;(lambda (x) (cis (* xmul x)))
	   ;(lambda (x) (+ foff (* fmul (cis (polynomial (* xmul (+ xoff x)) 1 -3 2 0)))))
	   (lambda (x) (+ foff (* fmul (cis (polynomial (* xmul (+ xoff x)) 1 0 0 0)))))
	   0 0
	   (complex (* 2 hw) 0)
	   4096)))
					;(draw-verts (mapcar (compose #'ccons #'cdr) (make-verts (compose (curry #'* hw 2/3) #'cis) 0 tau 6)))
       (save-png file))))
