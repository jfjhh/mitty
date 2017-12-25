;;;;
;;;; Value-based, extensible automatic differentiation.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter %ad-rules%
  '(((+ x y) (+ xd yd) (+ xdd ydd))
    ((* x y) (+ (* x yd) (* y xd)) (+ (* y xdd) (* 2 xd yd) (* x ydd)))
    ((/ x y) (/ (- (* y xd) (* x yd)) (* y y)) (/ (+ (* y y xdd) (* -1 y (+ (* 2 xd yd) (* x ydd))) (* 2 x yd yd)) (expt y 3)))
    ((sin x) (* xd (cos x)) (- (* xdd (cos x)) (* xd xd (sin x))))
    ((cos x) (* -1 xd (sin x)) (- (* xdd (sin x)) (* xd xd (cos x))))
    ((tan x) (* xd (tan x) (/ (cos x))) (* (expt (cos x) -2) (+ xdd (* 2 xd xd (tan x)))))
    ((expt x n) (* xd n (expt x (- n 1))) (* n (expt x (- n 2)) (+ (* x xdd) (* (- n 1) xd xd))))
    ((exp x) (* xd (exp x)) (* (exp x) (+ xdd (* xd xd))))))

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

(defun %defadrule% (func class &optional (fast nil))
  (let* ((rule (%adlookup% func))
	 (args (%adargs% (cdar rule) class))
	 (type (if fast 'double-float 'real)))
    (when args
      (compile (symbolicate "C" (format nil "~d" class) func)
	       `(lambda ,args
		  (declare (type ,type ,@args)
			   (optimize (speed 3) (debug 0) (compilation-speed 0)))
		  (values ,@(mapcar (curry #'list 'the type)
				    (subseq rule 0 (1+ class)))))))))

(dotimes (c (1+ 2))
  (mapc (lambda (x) (%defadrule% x c t)) %ad-funcs%))

(defun %ad-sexp% (sexp class wrt)
  (cond ((listp sexp)
	 (let ((func (find (car sexp) %ad-funcs%)))
	   (if func
	       `(multiple-value-call
		    #',(symbolicate "C" (format nil "~d" class) func)
		  ,@(mapcar (lambda (s) (%ad-sexp% s class wrt)) (cdr sexp)))
	       (mapcar (lambda (s) (%ad-sexp% s class wrt)) sexp))))
	((eq wrt sexp)
	 `(values ,@(subseq (list wrt 1d0 0d0) 0 (1+ class))))
	((or (numberp sexp) (symbolp sexp))
	 `(values ,@(subseq (list sexp 0d0 0d0) 0 (1+ class))))
	(t (format t "~a" sexp) sexp)))

(defmacro %adlambda% (class wrt &body body)
  `(lambda (,wrt)
     ,@(mapcar (lambda (s) (%ad-sexp% s class wrt)) body)))
