;;;;
;;;; Automatic Differentiation.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))

(defclass ad ()
  ((adval :initarg :adval
	:reader adval
	:type number)
   (adder :initarg :adder
	:reader adder
	:type number)))

(defmethod print-object ((object ad) stream)
  (format stream "#<AD ~a~%     ~a>" (adval object) (adder object)))

(defmethod ad ((adval number) &optional (adder 0))
  (make-instance 'ad :adval adval :adder adder))

(defmethod ad ((a ad) &optional b)
  (declare (ignore b))
  a)

(defmethod wrt ((a number))
  (ad a 1))

(defmethod adval ((a number)) a)

(defmethod adder ((a number)) 0)

(defparameter *%ads%*
  '((+    . ad-+)
    (-    . ad--)
    (*    . ad-*)
    (/    . ad-/)
    (exp  . ad-exp)
    (expt . ad-expt)
    (log  . ad-log)
    (sin  . ad-sin)
    (cos  . ad-cos)
    (tan  . ad-tan)
    (csc  . ad-csc)
    (sec  . ad-sec)
    (cot  . ad-cot)))

(defun %ad-sexp% (sexp)
  (cond ((symbolp sexp) (or (cdr (assoc sexp *%ads%*)) sexp))
	((numberp sexp) `(ad ,sexp 0))
	((atom sexp) sexp)
	(t (mapcar #'%ad-sexp% sexp))))

(defun %class-arg-lambda-list% (class lambda-list)
  (mapcar
   (lambda (s) (if (and (symbolp s)
		   (notany (the function (curry #'eq s)) lambda-list-keywords))
	      (list s class)
	      s))
   lambda-list))

(defmacro defadfun (name lambda-list &body body)
  `(defgeneric ,name ,lambda-list
     (:documentation ,(format nil "Automatically-differentiable function ~(~a~)." name))
     (:method ,lambda-list ,@body)
     (:method ,(%class-arg-lambda-list% 'ad lambda-list) ,@(%ad-sexp% body))))

(defmacro adlambda (args &body body)
  `(lambda ,args ,@(%ad-sexp% body)))

(defmethod ad-+ (&rest as)
  (ad (apply #'+ (mapcar #'adval as)) (apply #'+ (mapcar #'adder as))))

(defmethod ad-- (&rest as)
  (ad (apply #'- (mapcar #'adval as)) (apply #'- (mapcar #'adder as))))

(defun %ad-*% (aval ader bval bder)
  (ad (* aval bval)
      (+ (* aval bder) (* bval ader))))

(defmethod ad-* (&rest as)
  (let ((l (length as)))
    (if (< l 2)
	(ad (apply #'* (mapcar #'adval as))
	    (if (zerop l) 0 1))
	(let* ((a (car as))
	       (b (cadr as))
	       (p (%ad-*% (adval a) (adder a) (adval b) (adder b))))
	  (if (= l 2)
	      p
	      (apply #'ad-* p (cddr as)))))))

(defun %ad-/% (aval ader bval bder)
  (ad (/ aval bval)
      (/ (- (* bval ader) (* aval bder))
	 (* bval bval))))

(defmethod ad-/ ((a ad) &rest bs)
  (let ((l (length bs)))
    (cond ((zerop l)
	   (with-accessors ((adval adval) (adder adder)) a
	     (ad (/ adval)
		 (/ (- adder) (* adval adval)))))
	  ((= l 1)
	   (let ((b (car bs)))
	     (%ad-/% (adval a) (adder a) (adval b) (adder b))))
	  ((> l 1)
	   (let ((b (apply #'ad-* bs)))
	     (%ad-/% (adval a) (adder a) (adval b) (adder b)))))))

(defmethod ad-expt ((a ad) (b ad))
  (with-accessors ((aval adval) (ader adder)) a
    (with-accessors ((bval adval) (bder adder)) b
      (ad (expt aval bval)
	  (if (zerop bder)
	      (* bval (expt aval (- bval 1)) ader)
	      (* (expt aval (- bval 1))
		 (+ (* bval ader) (* aval (log (* aval bder))))))))))

(defmethod ad-exp ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (let ((v (exp adval)))
      (ad v
	  (* adder v)))))

(defmethod ad-log ((a ad) &optional b)
  (with-accessors ((aval adval) (ader adder)) a
    (if b
	(with-accessors ((bval adval) (bder adder)) b
	  (if (zerop bder)
	      (ad (log aval bval)
		  (/ ader bval aval))
	      (error "Too lazy to implement derivatives of log_g(x) f(x).")))
	(ad (log aval)
	    (/ ader aval)))))

(defun csc (number)
  (/ (sin number)))

(defun sec (number)
  (/ (cos number)))

(defun cot (number)
  (/ (tan number)))

(defmethod ad-sin ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (sin adval)
	(* adder (cos adval)))))

(defmethod ad-cos ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (cos adval)
	(* adder (- (sin adval))))))

(defmethod ad-tan ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (tan adval)
	(* adder (expt (cos adval) -2)))))

(defmethod ad-csc ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (csc adval)
	(* adder (cot adval) (csc adval)))))

(defmethod ad-sec ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (sec adval)
	(* adder (tan adval) (sec adval)))))

(defmethod ad-cot ((a ad))
  (with-accessors ((adval adval) (adder adder)) a
    (ad (cot adval)
	(* adder (- (expt (sin adval) -2))))))
