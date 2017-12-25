;;;;
;;;; Differential Geometry
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter +ralpha+ "α")

(defparameter +infinity-class+ -2)
(defparameter +analytic-class+ -1)

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
   (c :initform 0
      :initarg :c
      :accessor c
      :type (or integer symbol)
      :documentation "The differentiability class of the function.")
   (domain :initarg :domain
	   :accessor domain
	   :type interval
	   :documentation "The domain of the function."))
  (:metaclass c2mop:funcallable-standard-class))

(defun compile-rlambda (r)
  (with-slots (sfunc args func) r
    (setf func (compile nil `(lambda ,args
			       (declare (optimize (speed 1) (debug 3) (compilation-speed 0)))
			       ,@sfunc)))))

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

(defmacro rcompose (f g &optional (pos 0))
  (once-only ((f f) (g g) (pos pos))
    `(with-slots ((fargs args)) ,f
       (with-slots ((gargs args)) ,g
	 (let* ((sub (nth ,pos fargs))
		(fshadows (remove sub fargs)))
	   (mapc (lambda (a) (when (some (curry #'eq a) fshadows)
			  (%alpha-convert% a (gensym +ralpha+) ,g)))
		 gargs)
	   (let ((gfunc (sfunc ,g)))
	     (apply #'%rlambda%
		    (%flatten% (sublis `((,sub . ,gargs)) fargs))
		    (sublis
		     `((,sub . ,(if (not (cdr gfunc))
				    (car gfunc)
				    (cons 'progn gfunc))))
		     (sfunc ,f)))))))))

(defun %single-arg% (f)
  (with-slots (args sfunc) f
    (let ((arg (gensym +ralpha+)))
      (apply #'%rlambda% (list arg)
		 (sublis (mapcar (lambda (x) (cons x arg)) args) sfunc)))))

(defun %ngensyms% (n)
  (when (plusp n)
    (cons (gensym +ralpha+)
	  (%ngensyms% (1- n)))))

(defun rcombine (func &rest rs)
  (let* ((args (%ngensyms% (length rs)))
	 (base (%rlambda% args `(funcall ,func ,@args)))
	 (arg 0))
    (reduce (lambda (f g) (prog1 (rcompose f g arg) (incf arg)))
	    rs :initial-value base)))
