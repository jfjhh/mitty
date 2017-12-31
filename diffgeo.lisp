;;;;
;;;; Differential Geometry
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

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
   (func :initform nil
	 :initarg :func
	 :accessor func
	 :type function
	 :documentation "The funcallable real function.")
   (cfuncs :accessor cfuncs
	   :type list
	   :documentation "A list of AD functions of increasing differentiability class.")
   (c :initform *default-class*
      :initarg :c
      :accessor c
      :type (or integer symbol)
      :documentation "The differentiability class of the function.")
   (domain :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the function."))
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
  `(make-instance 'rlambda :args ',args :sfunc ',body :domain +full-interval+))

(defun %rlambda% (args &rest body)
  (make-instance 'rlambda :args args :sfunc body :domain +full-interval+))

(defmacro defrfun (name lambda-list &body body)
  `(progn
     (when (fboundp ',name)
       (warn "Redefining real function ~s." ',name))
     (setf (fdefinition ',name)
	   (rlambda ,lambda-list ,@body))
     ',name))

(defmacro defrfun* (name lambda-list &body body)
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

(dotimes (n *default-class*) (%defcncall% (1+ n)))

(defun cncall (function &rest arguments)
  (apply (%cnfunc% function (c function)) arguments))

(defun %defdncall% (n)
  (let ((name (format-symbol t "D~dCALL" n)))
    (compile name
	     `(lambda (function &rest arguments)
		(nth-value ,n (apply (nth ,(1- n) (cfuncs function)) arguments))))))

(dotimes (n *default-class*) (%defdncall% (1+ n)))

(defun dncall (function &rest arguments)
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

(defun %single-arg% (f)
  (with-slots (args sfunc) f
    (let ((arg (gensym +ralpha+)))
      (apply #'%rlambda% (list arg)
	     (sublis (mapcar (lambda (x) (cons x arg)) args) sfunc)))))

(defun %ngensyms% (n)
  (make-gensym-list n +ralpha+))

(defun rcombine (func &rest rs)
  (let* ((args (%ngensyms% (length rs)))
	 (base (%rlambda% args `(funcall ,func ,@args)))
	 (arg 0))
    (reduce (lambda (f g) (prog1 (rcompose f g arg) (incf arg)))
	    rs :initial-value base)))

(defun rlerp (a b)
  (let ((r (%rlambda% '(v) `(+ (* (- 1 v) ,a) (* v ,b)))))
    (setf (domain r) (interval 0 1))
    r)))
