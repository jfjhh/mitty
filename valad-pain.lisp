;;;;
;;;; Value-based, extensible automatic differentiation.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter %ad-rules%
  '(((+ x y) (+ xd yd) (+ xdd ydd))
    ((* x y) (+ (* x yd) (* y xd)) (+ (* x ydd) (* 2 xd yd) (* y xdd)))
    ((sin x) (* xd (cos x)) (* -1 xd xd (sin x)))
    ((cos x) (* -1 xd (sin x)) (* -1 xd xd (cos x)))))

(defparameter %ad-funcs%
  (mapcar #'caar %ad-rules%))

(defun %nrep% (x n)
  (when (plusp n)
    (cons x (%nrep% x (1- n)))))

(defun %adargs% (args class)
  (mapcan
      (lambda (x) (mapcar
	      (lambda (n) (apply #'symbolicate x (%nrep% 'd n)))
	      (iota (1+ class))))
      args))

(defun %adexp% (ad-rule class)
  (let ((rule (subseq ad-rule 0 (1+ class)))
	(args (cdar ad-rule)))
    (values
     (cons 'values rule)
     (%adargs% args class))))

(defun %adlookup% (s)
  (let ((pos (position s %ad-funcs%)))
    (when pos
      (nth pos %ad-rules%)))
  (find s %ad-rules% :test (lambda (x y) (eq x (caar y)))))

(defun %adize% (sexp class &optional (wrt 'u))
  "Turns sexp into a class-differentiable function in a single parameter wrt.
   Constructs function by composition, so may slow down with deeply nested sexps."
  (or
   (when (eq wrt sexp)
     (lambda (u &rest x)
       (declare (ignore x))
       (values-list (subseq (list u 1 0) 0 (1+ class)))))
   (when (numberp sexp)
     (lambda (&rest x)
       (declare (ignore x))
       (values-list (subseq (list sexp 0 0) 0 (1+ class)))))
   (and (listp sexp)
	(let ((rule (%adlookup% (car sexp))))
	  (when rule
	    (multiple-value-compose
	     (the function (%adlambda% rule class))
	     (lambda (&rest a)
	       (values-list
		(mapcan (lambda (s) (multiple-value-list
				(apply (%adize% s class) a)))
			(cdr sexp))))))))
   (error "Cannot AD sexp of class ~a wrt ~a: ~a" class wrt sexp)))

#||
TODO: (Macro or func?) to `let` a list like ((x 1) (xd 3)) ... but with gensyms:
(with-gensyms (x xd)
  `(let ((,x 1)
	 (,xd 3))
     (list (sin ,x) (* ,xd (cos ,x)) (* -1 ,xd ,xd (sin ,x)))))
||#

(defun %adcomp% (sexp class &optional (wrt 'u))
  (or
   (when (numberp sexp) (subseq (list sexp 0 0) 0 (1+ class)))
   (when (eq wrt sexp)  (subseq (list wrt 1 0) 0 (1+ class)))
   (and (listp sexp)
	(let ((rule (%adlookup% (car sexp))))
	  (when rule
	    (mapc (lambda (a) (%once-only-arg% a)) (cdr sexp))
	    rule)))
   (error "Cannot compute AD for sexp of class ~a wrt ~a: ~a" class wrt sexp)))

(defun %adlambda% (ad-rule class)
  (multiple-value-bind (body vars) (%adexp% ad-rule class)
    (compile nil
	     `(lambda ,vars
		(declare (optimize (speed 1) (debug 3) (compilation-speed 0)))
		,body))))
