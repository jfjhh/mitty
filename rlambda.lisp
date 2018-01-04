;;;;
;;;; Real Lambdas (Automatically-differentiable symbolic lambdas)
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defparameter +ralpha+ "α")

(defparameter +infinity-class+ -2)
(defparameter +analytic-class+ -1)

(defparameter *default-class* 2)

(defclass rlambda ()
  ((sfunc :initarg :sfunc
	  :accessor sfunc
	  :type list
	  :documentation "The symbolic expression of the function.")
   (args :initarg :args
	 :accessor args
	 :type list
	 :documentation "The bound variables of the symbolic function.")
   (func :initarg :func
	 :accessor func
	 :type function
	 :documentation "The funcallable real function.")
   (cfuncs :accessor cfuncs
	   :type list
	   :documentation "A list of AD functions of increasing differentiability class.")
   (c :initarg :c
      :accessor c
      :type (or integer symbol)
      :documentation "The differentiability class of the function.")
   (domain :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the function."))
  (:default-initargs :func nil :c *default-class*)
  (:metaclass c2mop:funcallable-standard-class))

(defun %funcall-rlambda-sexp% (sexp)
  (if (listp sexp)
      (let ((func (car sexp)))
	(if (and (symbolp func)
		 (boundp func)
		 (eq 'rlambda (type-of (eval func))))
	    `(funcall ,func ,@(mapcar #'%funcall-rlambda-sexp% (cdr sexp)))
	    (cons func (mapcar #'%funcall-rlambda-sexp% (cdr sexp)))))
      sexp))

(defun compile-rlambda (r)
  "(Re)compiles the callable functions of r to reflect the sexp inside."
  (with-slots (sfunc args func cfuncs c) r
    (setf cfuncs
	  (mapcar (lambda (s) (eval `(%adlambda% ,s ,(car args) ,@sfunc)))
		  (iota (if (plusp c) c 3) :start 1))) ; Do finitely many cfuncs for C^∞ or C^ω.
    (setf func (compile nil `(lambda ,args
			       (declare (optimize (speed 1) (debug 3) (compilation-speed 0)))
			       ,@(mapcar #'%funcall-rlambda-sexp% sfunc)))))) ; Like a Lisp-1.

(defmethod initialize-instance :after ((r rlambda) &key)
  (with-slots (sfunc args func domain) r
    (unless func
      (compile-rlambda r))
    (c2mop:set-funcallable-instance-function r func)))

(defun %class-string% (c)
  (case c
    (+infinity-class+ "∞")
    (+analytic-class+ "ω")
    (t (format nil "~d" c))))

(defmethod print-object ((r rlambda) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "of class ~a over ~a in ~(~a~):~%  ~{~(~a~)~^ ~}"
	    (%class-string% (c r)) (interval-string (domain r)) (args r) (sfunc r))))

(defmacro rlambda (args &body body)
  "Makes a rlambda, with similar syntax to normal lambdas."
  `(make-instance 'rlambda :args ',args :sfunc ',body :domain +full-interval+))

(defun %rlambda% (args &rest body)
  "Makes a rlambda, with similar syntax to normal lambdas, evaluating arguments."
  (make-instance 'rlambda :args args :sfunc body :domain +full-interval+))

(defmacro defrfun (name lambda-list &body body)
  "Defines a function, with similar syntax to defun, that is a rlambda."
  `(progn
     (when (fboundp ',name)
       (warn "Redefining real function ~s." ',name))
     (setf (fdefinition ',name)
	   (rlambda ,lambda-list ,@body))
     ',name))

(defun %cnfunc% (r &optional (n (c r)))
  (nth (1- n) (cfuncs r)))

(defun %defcncall% (n)
  (let ((name (format-symbol t "C~dCALL" n)))
    (compile name
	     `(lambda (function &rest arguments)
		(apply (nth ,(1- n) (cfuncs function)) arguments)))))

;;; Defines functions to do multiple-value returning calls of rlambdas of varying
;;; differentiability class. Example: to get the value and first and second derivatives
;;; of a C^2 rlambda as multiple values, one would use (c2call r 0.2d0).
(dotimes (n *default-class*) (%defcncall% (1+ n)))

(defun cncall (function &rest arguments)
  "Calls the highest differentiability class function of function (values)."
  (apply (%cnfunc% function (c function)) arguments))

(defun %defdncall% (n)
  (let ((name (format-symbol t "D~dCALL" n)))
    (compile name
	     `(lambda (function &rest arguments)
		(nth-value ,n (apply (nth ,(1- n) (cfuncs function)) arguments))))))

;;; Defines functions to do single-value returning calls of rlambdas of varying
;;; differentiability class. Example: to get the 2nd derivative of a C^2 rlambda,
;;; one would use (d2call r 0.2d0).
(dotimes (n *default-class*) (%defdncall% (1+ n)))

(defun dncall (function &rest arguments)
  "Calls the highest differentiability class function of function (last value)."
  (car (last (multiple-value-list
	      (apply (%cnfunc% function (c function)) arguments)))))

(defun %alpha-convert% (bound new rlambda)
  (with-slots (args sfunc) rlambda
    (let ((sub (list (cons bound new))))
      (setf args (sublis sub args))
      (setf sfunc (sublis sub sfunc)))))

(defun %flatten% (sexp &optional back acc)
  (cond ((consp sexp) (%flatten% (car sexp) (cons (cdr sexp) back) acc))
	(sexp (%flatten% (car back) (cdr back) (cons sexp acc)))
	(back (%flatten% (car back) (cdr back) acc))
	(t (nreverse acc))))

(defun rcompose (f g &optional (pos 0))
  "Composes rlambdae f and g into f.g, with the substitution at argument pos.
   Alpha-converts arguments as necessary."
  (with-slots ((fargs args)) f
    (with-slots ((gargs args)) g
      (let* ((sub (nth pos fargs))
	     (fshadows (remove sub fargs)))
	(dolist (a gargs)
	  (when (some (curry #'eq a) fshadows)
	    (%alpha-convert% a (gensym +ralpha+) g))
	  gargs)
	(let ((gfunc (sfunc g)))
	  (apply #'%rlambda%
		 (%flatten% (sublis `((,sub . ,gargs)) fargs))
		 (sublis
		  `((,sub . ,(if (not (cdr gfunc))
				 (car gfunc)
				 (cons 'progn gfunc))))
		  (sfunc f))))))))

(defun rlerp (a b)
  "Returns a rlambda that lerps between a and b."
  (let ((r (%rlambda% '(v) `(+ (* (- 1 v) ,a) (* v ,b)))))
    (setf (domain r) (interval 0d0 1d0))
    r))

;;; Generics of GSLL functions.
(defmethod evaluate ((object rlambda) point &key)
  (funcall object point))

(defmethod evaluate-derivative ((object rlambda) point &key)
  (d1call object point 1d0))

(defmethod evaluate-second-derivative ((object rlambda) point &key)
  (d2call object point 1d0 0d0))

(defmethod evaluate-derivative* ((object rlambda) point &key)
  (c1call object point 1d0))

(defmethod evaluate-second-derivative* ((object rlambda) point &key)
  (c2call object point 1d0 0d0))

(defmethod evaluate-integral ((object rlambda) lower-limit upper-limit &key)
  ;; Since RLAMBDAs are intended to be exact, use a high-order Gauss-Konrod rule.
  (integration-qag object lower-limit upper-limit :gauss61))
