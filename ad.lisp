;;;;
;;;; Automatic Differentiation.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defclass ad ()
  ((val :initarg :val
	:reader val
	:type number)
   (der :initarg :der
	:reader der
	:type number)))

(defmethod print-object ((object ad) stream)
  (format stream "#<AD ~a~%     ~a>" (val object) (der object)))

(defmethod ad ((val number) &optional (der 0))
  (make-instance 'ad :val val :der der))

(defmethod ad ((a ad) &optional b)
  (declare (ignore b))
  a)

(defmethod wrt ((a number))
  (ad a 1))

(defmethod val ((a number)) a)

(defmethod der ((a number)) 0)

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
		   (notany (curry #'eq s) lambda-list-keywords))
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
  (ad (apply #'+ (mapcar #'val as)) (apply #'+ (mapcar #'der as))))

(defmethod ad-- (&rest as)
  (ad (apply #'- (mapcar #'val as)) (apply #'- (mapcar #'der as))))

(defun %ad-*% (aval ader bval bder)
  (ad (* aval bval)
      (+ (* aval bder) (* bval ader))))

(defmethod ad-* (&rest as)
  (let ((l (length as)))
    (if (< l 2)
	(ad (apply #'* (mapcar #'val as))
	    (if (zerop l) 0 1))
	(let* ((a (car as))
	       (b (cadr as))
	       (p (%ad-*% (val a) (der a) (val b) (der b))))
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
	   (with-accessors ((val val) (der der)) a
	     (ad (/ val)
		 (/ (- der) (* val val)))))
	  ((= l 1)
	   (let ((b (car bs)))
	     (%ad-/% (val a) (der a) (val b) (der b))))
	  ((> l 1)
	   (let ((b (apply #'ad-* bs)))
	     (%ad-/% (val a) (der a) (val b) (der b)))))))

(defmethod ad-expt ((a ad) (b ad))
  (with-accessors ((aval val) (ader der)) a
    (with-accessors ((bval val) (bder der)) b
      (ad (expt aval bval)
	  (if (zerop bder)
	      (* bval (expt aval (- bval 1)) ader)
	      (* (expt aval (- bval 1))
		 (+ (* bval ader) (* aval (log (* aval bder))))))))))

(defmethod ad-exp ((a ad))
  (with-accessors ((val val) (der der)) a
    (let ((v (exp val)))
      (ad v
	  (* der v)))))

(defmethod ad-log ((a ad) &optional b)
  (with-accessors ((aval val) (ader der)) a
    (if b
	(with-accessors ((bval val) (bder der)) b
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
  (with-accessors ((val val) (der der)) a
    (ad (sin val)
	(* der (cos val)))))

(defmethod ad-cos ((a ad))
  (with-accessors ((val val) (der der)) a
    (ad (cos val)
	(* der (- (sin val))))))

(defmethod ad-tan ((a ad))
  (with-accessors ((val val) (der der)) a
    (ad (tan val)
	(* der (expt (cos val) -2)))))

(defmethod ad-csc ((a ad))
  (with-accessors ((val val) (der der)) a
    (ad (csc val)
	(* der (cot val) (csc val)))))

(defmethod ad-sec ((a ad))
  (with-accessors ((val val) (der der)) a
    (ad (sec val)
	(* der (tan val) (sec val)))))

(defmethod ad-cot ((a ad))
  (with-accessors ((val val) (der der)) a
    (ad (cot val)
	(* der (- (expt (sin val) -2))))))
