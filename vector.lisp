;;;;
;;;; Basic vector functions.
;;;; Alex Striff
;;;;

(in-package #:mitty)

(defmacro %vn! (n)
  (let* ((nn (eval n))
	 (name (intern (format nil "V~a!" nn))))
    `(defun ,name (&rest args)
       ;(declare (optimize (speed 3) (safety 1) (compilation-speed 0)))
       (declare (type cons args))
       (if (= (length args) ,nn)
	   (make-array ,nn
		       :element-type 'single-float
		       :initial-contents args)
	   (error "v~a! was called with ~a arguments." ,nn (length args))))))

(defmacro %vn*! (n)
  (let* ((nn (eval n))
	 (name (intern (format nil "V~a*!" nn))))
    `(defun ,name (&rest args)
       ;(declare (optimize (speed 3) (safety 1) (compilation-speed 0)))
       (declare (type cons args))
       (if (= (length args) ,nn)
	   (make-array ,nn
		       :element-type 'single-float
		       :initial-contents (mapcar #'float args))
	   (error "v~a! was called with ~a arguments." ,nn (length args))))))

(defmacro %vn (n)
  (let* ((nn (eval n))
	 (name (intern (format nil "V~a" nn))))
    `(defun ,name (&rest args)
       ;(declare (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 3)))
       (declare (type cons args))
       (let ((elems (append args (cmat (- ,nn (length args)) 0f0))))
	 (make-array ,nn
		     :element-type 'single-float
		     :initial-contents elems)))))

(defmacro %vn* (n)
  (let* ((nn n)
	 (name (intern (format nil "V~a*" nn))))
    `(defun ,name (&rest args)
       ;(declare (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 3)))
       (declare (type cons args))
       (let ((elems (append args (cmat (- ,nn (length args)) 0f0))))
	 (make-array ,nn
		     :element-type 'single-float
		     :initial-contents (mapcar #'float elems))))))

;; This hurts. Doing this pragmatically proved difficult. Fixme?
(%vn!  0)
(%vn*! 0)
(%vn   0)
(%vn*  0)
(%vn!  1)
(%vn*! 1)
(%vn   1)
(%vn*  1)
(%vn!  2)
(%vn*! 2)
(%vn   2)
(%vn*  2)
(%vn!  3)
(%vn*! 3)
(%vn   3)
(%vn*  3)
(%vn!  4)
(%vn*! 4)
(%vn   4)
(%vn*  4)

(defun v! (&rest args)
  (make-array (length args) :initial-contents (mapcar #'float args)))

(defun v (&rest args)
  (make-array (length args) :initial-contents args))

(defmacro def-vec-func (f name)
  `(defun ,name (&rest vecs) 
     (let ((len (length vecs)))
       (apply (curry #'map 'vector ,f)
	      (mapcar (lambda (v) (if (numberp v) (nrep len v) v))
		      vecs)))))

(defmacro def-vec-vec-func (f name)
  `(defun ,name (vec-vecs) 
     (let ((len (length vec-vecs)))
       (reduce ,f (map 'vector
		       (lambda (v) (if (numberp v) (nrep len v) v))
		       vec-vecs)))))

;;; Product functions for arithmetic.
(def-vec-func #'+ v+)
(def-vec-func #'- v-)
(def-vec-func #'* v*)
(def-vec-func #'/ v/)
(def-vec-func (lambda (&rest args) (make-array (length args) :initial-contents args)) vt)

;;; Arithmetic on vectors of vectors.
(def-vec-vec-func #'v+ vv+)
(def-vec-vec-func #'v- vv-)
(def-vec-vec-func #'v* vv*)
(def-vec-vec-func #'v/ vv/)

(defun vvt (vec-vecs)
  (let* ((rows (length (elt vec-vecs 0)))
	 (out (make-array rows :fill-pointer 0)))
    (dotimes (i rows)
      (vector-push-extend (map 'vector (lambda (v) (elt v i)) vec-vecs) out))
    out))

(defun nvec (n &optional (x 0))
  (let ((v (make-array n)))
    (dotimes (i n)
      (setf (elt v i) (+ i x)))
    v))

(defun nrep (n &optional (x 0))
  (make-array n :initial-element x))

(defun sv* (s v)
  (map 'vector (curry #'* s) v))

(defun mv* (a x)
  "Multiplies a matrix by a vector."
  (vv+ (map 'vector #'v* a (map 'vector (curry #'nrep (length a)) x))))

(defun mm* (a b)
  "Multiplies a matrix by a matrix."
  (map 'vector (curry #'mv* a) b))

(defun sm* (s m)
  (map 'vector (curry #'sv* s) m))

(defun cmat (n &optional (c 0))
  (nrep n (nrep n c)))

(defun print-mat (a)
  (format t "#<MAT~%")
  (let ((at (vvt a)))
    (dotimes (i (length at))
      (let ((v (elt at i)))
	(dotimes (j (length v))
	  (format t "~2d " (elt v j)))
	(terpri))))
  (format t ">~%"))
