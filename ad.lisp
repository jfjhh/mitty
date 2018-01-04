;;;;
;;;; Multiple-value-based, extensible automatic differentiation.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defparameter *max-class* 2)
(defparameter *adrule-fast* t)

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

(defparameter %ad-nargs%
  '(+ *))

(defparameter %ad-nargs-ids%
  '(0 1))

(defun %nrep% (x n)
  (when (plusp n)
    (cons x (%nrep% x (1- n)))))

(defun %nest-nargs% (sexp &optional (binary t))
  (cond ((cddr sexp)
	 (append (list (car sexp) (cadr sexp))
		 (list (%nest-nargs% (cons (car sexp) (cddr sexp))))))
	((cdr sexp)
	 (if binary (cadr sexp) sexp))
	(t
	 (let ((pos (position (car sexp) %ad-nargs%)))
	   (and pos (nth pos %ad-nargs-ids%))))))

(defun %ad-nsexp% (sexp)
  (if (and sexp (listp sexp))
      (let ((new
	     (if (some (curry #'eq (car sexp)) %ad-nargs%)
		 (%nest-nargs% sexp)
		 sexp)))
	(cons (car new) (mapcar #'%ad-nsexp% (cdr new))))
      sexp))

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

(defun %defadrule% (func class &optional (fast *adrule-fast*))
  (let* ((rule (%adlookup% func))
	 (args (%adargs% (cdar rule) class))
	 (type (if fast 'double-float 'real))
	 (name (format-symbol t "C~d~a" class func)))
    (when args
      (compile name
	       `(lambda ,args
		  (declare (type ,type ,@args)
			   (optimize (speed 3) (debug 0) (compilation-speed 0)))
		  (values ,@(mapcar (curry #'list 'the type)
				    (subseq rule 0 (1+ class)))))))))

(defun %ad-diff% (sexp class wrt)
  (cond
    ((listp sexp)
     (let* ((fsym (car sexp))
	    (func (find fsym %ad-funcs%))
	    (rbound (and (symbolp fsym)
			 (boundp fsym)
			 (eq 'rlambda (type-of (eval fsym))))))
       (if (or rbound func)
	   `(multiple-value-call
		,(if rbound
		     `(car (last (cfuncs (eval ,fsym))))
		     `(fdefinition ',(format-symbol t "C~d~a" class func)))
	      ,@(mapcar (lambda (s) (%ad-diff% s class wrt)) (cdr sexp)))
	   (cons (car sexp)
		 (mapcar (lambda (s) (%ad-diff% s class wrt)) (cdr sexp))))))
    ((eq wrt sexp)
     `(values ,@(%adargs% (list wrt) class)))
    ((or (numberp sexp))
     `(values ,@(subseq (list sexp 0d0 0d0) 0 (1+ class))))
    (t (warn "AD stage %ad-diff% cannot process sexp ~a" sexp)
       sexp)))

(defun %ad-sexp% (sexp class wrt)
  (%ad-diff% (%ad-nsexp% sexp) class wrt))

(defmacro %adlambda% (class wrt &body body)
  `(lambda ,(%adargs% (list wrt) class)
     ,@(mapcar (lambda (s) (%ad-sexp% s class wrt)) body)))

(defmacro %adlambda*% (class wrt &body body)
  `(lambda (,wrt)
     (multiple-value-call (%adlambda% ,class ,wrt ,@body) (wrt ,wrt ,class))))

(declaim (inline wrt))
(defun wrt (x &optional (c *max-class*))
  (values-list
   (subseq (append (list x 1) (%nrep% 0 (- *max-class* 1)))
	   0 (1+ c))))

(dotimes (c (1+ *max-class*))
  (dolist (x %ad-funcs%)
    (%defadrule% x c)))
