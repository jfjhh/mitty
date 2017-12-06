;;;;
;;;; Base definitions and functions.
;;;; Alex Striff.
;;;;

(in-package #:mitty)

(defconstant tau (* 2 pi))

(defparameter *width* 512)
(defparameter *height* 512)

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

(defun draw-verts (verts)
  (set-rgba-fill 1 1 1 1)
  (set-rgba-stroke 1 1 1 1)
  (set-line-width 1)
  (loop :for v :in verts :do
     (let ((x (car v))
	   (y (cdr v)))
       (centered-circle-path x y 0.75)
       (fill-path))))

(defun polynomial (&rest coeffs)
  (let ((deg -1)
	(varsym (gensym)))
    (eval (list 'lambda (list varsym)
		'(declare (optimize (speed 3) (safety 1)))
		(cons '+
		      (mapcar (lambda (c) (incf deg) `(* ,c (expt ,varsym ,deg)))
			      (reverse coeffs)))))))

(defun test-curves (file)
  (with-canvas (:width *width* :height *height*)
    (let ((hw (/ *width* 2))
	  (hh (/ *height* 2)))
      (translate hw hh)
      (set-rgba-stroke 0 0 0 0.25)
      (move-to 0 hh)
      (line-to 0 (- hh))
      (stroke)
      (move-to (- hw) 0)
      (line-to hw 0)
      (stroke)
      (draw-verts (make-bullet (lambda (x) (let ((s (* x 3)))
					(* 40 (cis
					       (funcall (polynomial 1 -3 2 0)
							(* (1+ s) 15))))))
			       0 (* 1/4 (- hh))
			       (complex (* 2 hw) 0)
			       2048))
					;(draw-verts (mapcar (compose #'ccons #'cdr) (make-verts (compose (curry #'* hw 2/3) #'cis) 0 tau 6)))
      (save-png file))))
