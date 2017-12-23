;;;;
;;;; Real Interval Tests
;;;; Alex Striff
;;;;

(in-package #:mitty)

(declaim (optimize (speed 1) (safety 1) (debug 3) (compilation-speed 0)))

(defparameter %test-interval=-intervals%
  '(;; Same endpoint equality.
    (((0 1 t   t  ) . (0 1 t   t  )) . (t   t  ))
    (((0 1 nil t  ) . (0 1 t   t  )) . (nil t  ))
    (((0 1 t   nil) . (0 1 t   t  )) . (nil t  ))
    (((0 1 nil nil) . (0 1 t   t  )) . (nil t  ))
    (((0 1 t   t  ) . (0 1 t   t  )) . (t   t  ))
    (((0 1 t   t  ) . (0 1 nil t  )) . (nil t  ))
    (((0 1 t   t  ) . (0 1 t   nil)) . (nil t  ))
    (((0 1 t   t  ) . (0 1 nil nil)) . (nil t  ))
    ;; Different endpoint equality.
    (((0 1 t   t  ) . (0 2 t   t  )) . (nil nil))
    (((0 1 nil t  ) . (0 2 t   t  )) . (nil nil))
    (((0 1 t   nil) . (0 2 t   t  )) . (nil nil))
    (((0 1 nil nil) . (0 2 t   t  )) . (nil nil))
    (((0 2 t   t  ) . (0 1 t   t  )) . (nil nil))
    (((0 2 t   t  ) . (0 1 nil t  )) . (nil nil))
    (((0 2 t   t  ) . (0 1 t   nil)) . (nil nil))
    (((0 2 t   t  ) . (0 1 nil nil)) . (nil nil))
    ;; Empty interval equality.
    (((0 1 t   t  )    . +empty-interval+) . (nil nil))
    (((0 1 nil t  )    . +empty-interval+) . (nil nil))
    (((0 1 t   nil)    . +empty-interval+) . (nil nil))
    (((0 1 nil nil)    . +empty-interval+) . (nil nil))
    ((+empty-interval+ . (0 1 t   t  ))    . (nil nil))
    ((+empty-interval+ . (0 1 nil t  ))    . (nil nil))
    ((+empty-interval+ . (0 1 t   nil))    . (nil nil))
    ((+empty-interval+ . (0 1 nil nil))    . (nil nil))
    ((+empty-interval+ . +empty-interval+) . (t   t  ))))

(lisp-unit:define-test interval=
  (prog1 t
    (mapc
     (lambda (test-list)
       (let* ((test-intervals (car test-list))
	      (target (cdr test-list))
	      (left (car test-intervals))
	      (right (cdr test-intervals)))
	 (lisp-unit:assert-true
	  (equal
	   (multiple-value-list
	    (interval=
	     (if (eq left '+empty-interval+)
		 +empty-interval+
		 (apply #'interval left))
	     (if (eq right '+empty-interval+)
		 +empty-interval+
		 (apply #'interval right))))
	   target))))
     %test-interval=-intervals%)))

(defparameter %test-unite-intervals%
  '(;; Overlapping Interval Unions.
    (((0 2 t   t  ) . (1 3 t   t  )) . (0 3 t   t  ))
    (((0 2 nil t  ) . (1 3 t   t  )) . (0 3 nil t  ))
    (((0 2 t   nil) . (1 3 t   t  )) . (0 3 t   t  ))
    (((0 2 nil nil) . (1 3 t   t  )) . (0 3 nil t  ))
    (((0 2 t   t  ) . (1 3 nil t  )) . (0 3 t   t  ))
    (((0 2 nil t  ) . (1 3 nil t  )) . (0 3 nil t  ))
    (((0 2 t   nil) . (1 3 nil t  )) . (0 3 t   t  ))
    (((0 2 nil nil) . (1 3 nil t  )) . (0 3 nil t  ))
    (((0 2 t   t  ) . (1 3 t   nil)) . (0 3 t   nil))
    (((0 2 nil t  ) . (1 3 t   nil)) . (0 3 nil nil))
    (((0 2 t   nil) . (1 3 t   nil)) . (0 3 t   nil))
    (((0 2 nil nil) . (1 3 t   nil)) . (0 3 nil nil))
    (((0 2 t   t  ) . (1 3 nil nil)) . (0 3 t   nil))
    (((0 2 nil t  ) . (1 3 nil nil)) . (0 3 nil nil))
    (((0 2 t   nil) . (1 3 nil nil)) . (0 3 t   nil))
    (((0 2 nil nil) . (1 3 nil nil)) . (0 3 nil nil))
    ;; Overlapping Interval Unions (Reversed).
    (((1 3 t   t  ) . (0 2 t   t  )) . (0 3 t   t  ))
    (((1 3 t   t  ) . (0 2 nil t  )) . (0 3 nil t  ))
    (((1 3 t   t  ) . (0 2 t   nil)) . (0 3 t   t  ))
    (((1 3 t   t  ) . (0 2 nil nil)) . (0 3 nil t  ))
    (((1 3 nil t  ) . (0 2 t   t  )) . (0 3 t   t  ))
    (((1 3 nil t  ) . (0 2 nil t  )) . (0 3 nil t  ))
    (((1 3 nil t  ) . (0 2 t   nil)) . (0 3 t   t  ))
    (((1 3 nil t  ) . (0 2 nil nil)) . (0 3 nil t  ))
    (((1 3 t   nil) . (0 2 t   t  )) . (0 3 t   nil))
    (((1 3 t   nil) . (0 2 nil t  )) . (0 3 nil nil))
    (((1 3 t   nil) . (0 2 t   nil)) . (0 3 t   nil))
    (((1 3 t   nil) . (0 2 nil nil)) . (0 3 nil nil))
    (((1 3 nil nil) . (0 2 t   t  )) . (0 3 t   nil))
    (((1 3 nil nil) . (0 2 nil t  )) . (0 3 nil nil))
    (((1 3 nil nil) . (0 2 t   nil)) . (0 3 t   nil))
    (((1 3 nil nil) . (0 2 nil nil)) . (0 3 nil nil))
    ;; Osculating Interval Unions.
    (((0 1 t   t  ) . (1 2 t   t  )) . (0 2 t   t  ))
    (((0 1 nil t  ) . (1 2 t   t  )) . (0 2 nil t  ))
    (((0 1 t   nil) . (1 2 t   t  )) . (0 2 t   t  ))
    (((0 1 nil nil) . (1 2 t   t  )) . (0 2 nil t  ))
    (((0 1 t   t  ) . (1 2 nil t  )) . (0 2 t   t  ))
    (((0 1 nil t  ) . (1 2 nil t  )) . (0 2 nil t  ))
    (((0 1 t   nil) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 nil nil) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 t   t  ) . (1 2 t   nil)) . (0 2 t   nil))
    (((0 1 nil t  ) . (1 2 t   nil)) . (0 2 nil nil))
    (((0 1 t   nil) . (1 2 t   nil)) . (0 2 t   nil))
    (((0 1 nil nil) . (1 2 t   nil)) . (0 2 nil nil))
    (((0 1 t   t  ) . (1 2 nil nil)) . (0 2 t   nil))
    (((0 1 nil t  ) . (1 2 nil nil)) . (0 2 nil nil))
    (((0 1 t   nil) . (1 2 nil nil)) . +empty-interval+)
    (((0 1 nil nil) . (1 2 nil nil)) . +empty-interval+)
    ;; Osculating Interval Unions (Reversed).
    (((1 2 t   t  ) . (0 1 t   t  )) . (0 2 t   t  ))
    (((1 2 t   t  ) . (0 1 nil t  )) . (0 2 nil t  ))
    (((1 2 t   t  ) . (0 1 t   nil)) . (0 2 t   t  ))
    (((1 2 t   t  ) . (0 1 nil nil)) . (0 2 nil t  ))
    (((1 2 nil t  ) . (0 1 t   t  )) . (0 2 t   t  ))
    (((1 2 nil t  ) . (0 1 nil t  )) . (0 2 nil t  ))
    (((1 2 nil t  ) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 nil t  ) . (0 1 nil nil)) . +empty-interval+)
    (((1 2 t   nil) . (0 1 t   t  )) . (0 2 t   nil))
    (((1 2 t   nil) . (0 1 nil t  )) . (0 2 nil nil))
    (((1 2 t   nil) . (0 1 t   nil)) . (0 2 t   nil))
    (((1 2 t   nil) . (0 1 nil nil)) . (0 2 nil nil))
    (((1 2 nil nil) . (0 1 t   t  )) . (0 2 t   nil))
    (((1 2 nil nil) . (0 1 nil t  )) . (0 2 nil nil))
    (((1 2 nil nil) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 nil nil) . (0 1 nil nil)) . +empty-interval+)
    ;; Separated Interval Unions.
    (((0 1 t   t  ) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 t   nil) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 nil nil) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 t   nil) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 nil nil) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 t   nil) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 nil nil) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 t   nil) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 nil nil) . (2 3 nil nil)) . +empty-interval+)
    ;; Separated Interval Unions (Reversed).
    (((2 3 t   t  ) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 t   nil) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 t   nil) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 t   nil) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 t   nil) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 nil nil) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 nil nil) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 nil nil) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 nil nil) . (0 1 nil nil)) . +empty-interval+)
    ;; Equal Interval Unions.
    (((0 1 t   t  ) . (0 1 t   t  )) . (0 1 t   t  ))
    (((0 1 nil t  ) . (0 1 t   t  )) . (0 1 t   t  ))
    (((0 1 t   nil) . (0 1 t   t  )) . (0 1 t   t  ))
    (((0 1 nil nil) . (0 1 t   t  )) . (0 1 t   t  ))
    (((0 1 t   t  ) . (0 1 nil t  )) . (0 1 t   t  ))
    (((0 1 nil t  ) . (0 1 nil t  )) . (0 1 nil t  ))
    (((0 1 t   nil) . (0 1 nil t  )) . (0 1 t   t  ))
    (((0 1 nil nil) . (0 1 nil t  )) . (0 1 nil t  ))
    (((0 1 t   t  ) . (0 1 t   nil)) . (0 1 t   t  ))
    (((0 1 nil t  ) . (0 1 t   nil)) . (0 1 t   t  ))
    (((0 1 t   nil) . (0 1 t   nil)) . (0 1 t   nil))
    (((0 1 nil nil) . (0 1 t   nil)) . (0 1 t   nil))
    (((0 1 t   t  ) . (0 1 nil nil)) . (0 1 t   t  ))
    (((0 1 nil t  ) . (0 1 nil nil)) . (0 1 nil t  ))
    (((0 1 t   nil) . (0 1 nil nil)) . (0 1 t   nil))
    (((0 1 nil nil) . (0 1 nil nil)) . (0 1 nil nil))
    ;; Empty Interval Unions.
    (((0 1 t   t  )    . +empty-interval+) . (0 1 t   t  ))
    ((+empty-interval+ . (0 1 t   t  ))    . (0 1 t   t  ))
    (((0 1 nil t  )    . +empty-interval+) . (0 1 nil t  ))
    ((+empty-interval+ . (0 1 nil t  ))    . (0 1 nil t  ))
    (((0 1 t   nil)    . +empty-interval+) . (0 1 t   nil))
    ((+empty-interval+ . (0 1 t   nil))    . (0 1 t   nil))
    (((0 1 nil nil)    . +empty-interval+) . (0 1 nil nil))
    ((+empty-interval+ . (0 1 nil nil))    . (0 1 nil nil))
    ((+empty-interval+ . +empty-interval+) . +empty-interval+)))

(lisp-unit:define-test unite
  (prog1 t
    (mapc
     (lambda (test-list)
       (let* ((test-intervals (car test-list))
	      (target (cdr test-list))
	      (left (car test-intervals))
	      (right (cdr test-intervals)))
	 (lisp-unit:assert-true
	  (interval=
	   (unite
	    (if (eq left '+empty-interval+)
		+empty-interval+
		(apply #'interval left))
	    (if (eq right '+empty-interval+)
		+empty-interval+
		(apply #'interval right)))
	   (if (eq target '+empty-interval+)
	       +empty-interval+
	       (apply #'interval target))))))
     %test-union-intervals%)))

(defparameter %test-intersect-intervals%
  '(;; Overlapping Interval Intersections.
    (((0 2 t   t  ) . (1 3 t   t  )) . (1 2 t   t  ))
    (((0 2 nil t  ) . (1 3 t   t  )) . (1 2 t   t  ))
    (((0 2 t   nil) . (1 3 t   t  )) . (1 2 t   nil))
    (((0 2 nil nil) . (1 3 t   t  )) . (1 2 t   nil))
    (((0 2 t   t  ) . (1 3 nil t  )) . (1 2 nil t  ))
    (((0 2 nil t  ) . (1 3 nil t  )) . (1 2 nil t  ))
    (((0 2 t   nil) . (1 3 nil t  )) . (1 2 nil nil))
    (((0 2 nil nil) . (1 3 nil t  )) . (1 2 nil nil))
    (((0 2 t   t  ) . (1 3 t   nil)) . (1 2 t   t  ))
    (((0 2 nil t  ) . (1 3 t   nil)) . (1 2 t   t  ))
    (((0 2 t   nil) . (1 3 t   nil)) . (1 2 t   nil))
    (((0 2 nil nil) . (1 3 t   nil)) . (1 2 t   nil))
    (((0 2 t   t  ) . (1 3 nil nil)) . (1 2 nil t  ))
    (((0 2 nil t  ) . (1 3 nil nil)) . (1 2 nil t  ))
    (((0 2 t   nil) . (1 3 nil nil)) . (1 2 nil nil))
    (((0 2 nil nil) . (1 3 nil nil)) . (1 2 nil nil))
    ;; Overlapping Interval Intersections (Reversed).
    (((1 3 t   t  ) . (0 2 t   t  )) . (1 2 t   t  ))
    (((1 3 t   t  ) . (0 2 nil t  )) . (1 2 t   t  ))
    (((1 3 t   t  ) . (0 2 t   nil)) . (1 2 t   nil))
    (((1 3 t   t  ) . (0 2 nil nil)) . (1 2 t   nil))
    (((1 3 nil t  ) . (0 2 t   t  )) . (1 2 nil t  ))
    (((1 3 nil t  ) . (0 2 nil t  )) . (1 2 nil t  ))
    (((1 3 nil t  ) . (0 2 t   nil)) . (1 2 nil nil))
    (((1 3 nil t  ) . (0 2 nil nil)) . (1 2 nil nil))
    (((1 3 t   nil) . (0 2 t   t  )) . (1 2 t   t  ))
    (((1 3 t   nil) . (0 2 nil t  )) . (1 2 t   t  ))
    (((1 3 t   nil) . (0 2 t   nil)) . (1 2 t   nil))
    (((1 3 t   nil) . (0 2 nil nil)) . (1 2 t   nil))
    (((1 3 nil nil) . (0 2 t   t  )) . (1 2 nil t  ))
    (((1 3 nil nil) . (0 2 nil t  )) . (1 2 nil t  ))
    (((1 3 nil nil) . (0 2 t   nil)) . (1 2 nil nil))
    (((1 3 nil nil) . (0 2 nil nil)) . (1 2 nil nil))
    ;; Osculating Interval Intersections.
    (((0 1 t   t  ) . (1 2 t   t  )) . (1 1 t   t  ))
    (((0 1 nil t  ) . (1 2 t   t  )) . (1 1 t   t  ))
    (((0 1 t   nil) . (1 2 t   t  )) . +empty-interval+)
    (((0 1 nil nil) . (1 2 t   t  )) . +empty-interval+)
    (((0 1 t   t  ) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 nil t  ) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 t   nil) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 nil nil) . (1 2 nil t  )) . +empty-interval+)
    (((0 1 t   t  ) . (1 2 t   nil)) . (1 1 t   t  ))
    (((0 1 nil t  ) . (1 2 t   nil)) . (1 1 t   t  ))
    (((0 1 t   nil) . (1 2 t   nil)) . +empty-interval+)
    (((0 1 nil nil) . (1 2 t   nil)) . +empty-interval+)
    (((0 1 t   t  ) . (1 2 nil nil)) . +empty-interval+)
    (((0 1 nil t  ) . (1 2 nil nil)) . +empty-interval+)
    (((0 1 t   nil) . (1 2 nil nil)) . +empty-interval+)
    (((0 1 nil nil) . (1 2 nil nil)) . +empty-interval+)
    ;; Osculating Interval Intersections (Reversed).
    (((1 2 t   t  ) . (0 1 t   t  )) . (1 1 t   t  ))
    (((1 2 t   t  ) . (0 1 nil t  )) . (1 1 t   t  ))
    (((1 2 t   t  ) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 t   t  ) . (0 1 nil nil)) . +empty-interval+)
    (((1 2 nil t  ) . (0 1 t   t  )) . +empty-interval+)
    (((1 2 nil t  ) . (0 1 nil t  )) . +empty-interval+)
    (((1 2 nil t  ) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 nil t  ) . (0 1 nil nil)) . +empty-interval+)
    (((1 2 t   nil) . (0 1 t   t  )) . (1 1 t   t  ))
    (((1 2 t   nil) . (0 1 nil t  )) . (1 1 t   t  ))
    (((1 2 t   nil) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 t   nil) . (0 1 nil nil)) . +empty-interval+)
    (((1 2 nil nil) . (0 1 t   t  )) . +empty-interval+)
    (((1 2 nil nil) . (0 1 nil t  )) . +empty-interval+)
    (((1 2 nil nil) . (0 1 t   nil)) . +empty-interval+)
    (((1 2 nil nil) . (0 1 nil nil)) . +empty-interval+)
    ;; Separated Interval Intersections.
    (((0 1 t   t  ) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 t   nil) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 nil nil) . (2 3 t   t  )) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 t   nil) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 nil nil) . (2 3 nil t  )) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 t   nil) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 nil nil) . (2 3 t   nil)) . +empty-interval+)
    (((0 1 t   t  ) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 nil t  ) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 t   nil) . (2 3 nil nil)) . +empty-interval+)
    (((0 1 nil nil) . (2 3 nil nil)) . +empty-interval+)
    ;; Separated Interval Intersections (Reversed).
    (((2 3 t   t  ) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 t   t  ) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 nil t  ) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 t   nil) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 t   nil) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 t   nil) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 t   nil) . (0 1 nil nil)) . +empty-interval+)
    (((2 3 nil nil) . (0 1 t   t  )) . +empty-interval+)
    (((2 3 nil nil) . (0 1 nil t  )) . +empty-interval+)
    (((2 3 nil nil) . (0 1 t   nil)) . +empty-interval+)
    (((2 3 nil nil) . (0 1 nil nil)) . +empty-interval+)
    ;; Equal Interval Intersections.
    (((0 1 t   t  ) . (0 1 t   t  )) . (0 1 t   t  ))
    (((0 1 nil t  ) . (0 1 t   t  )) . (0 1 nil t  ))
    (((0 1 t   nil) . (0 1 t   t  )) . (0 1 t   nil))
    (((0 1 nil nil) . (0 1 t   t  )) . (0 1 nil nil))
    (((0 1 t   t  ) . (0 1 nil t  )) . (0 1 nil t  ))
    (((0 1 nil t  ) . (0 1 nil t  )) . (0 1 nil t  ))
    (((0 1 t   nil) . (0 1 nil t  )) . (0 1 nil nil))
    (((0 1 nil nil) . (0 1 nil t  )) . (0 1 nil nil))
    (((0 1 t   t  ) . (0 1 t   nil)) . (0 1 t   nil))
    (((0 1 nil t  ) . (0 1 t   nil)) . (0 1 nil nil))
    (((0 1 t   nil) . (0 1 t   nil)) . (0 1 t   nil))
    (((0 1 nil nil) . (0 1 t   nil)) . (0 1 nil nil))
    (((0 1 t   t  ) . (0 1 nil nil)) . (0 1 nil nil))
    (((0 1 nil t  ) . (0 1 nil nil)) . (0 1 nil nil))
    (((0 1 t   nil) . (0 1 nil nil)) . (0 1 nil nil))
    (((0 1 nil nil) . (0 1 nil nil)) . (0 1 nil nil))
    ;; Empty Interval Unions.
    (((0 1 t   t  )    . +empty-interval+) . +empty-interval+)
    ((+empty-interval+ . (0 1 t   t  ))    . +empty-interval+)
    (((0 1 nil t  )    . +empty-interval+) . +empty-interval+)
    ((+empty-interval+ . (0 1 nil t  ))    . +empty-interval+)
    (((0 1 t   nil)    . +empty-interval+) . +empty-interval+)
    ((+empty-interval+ . (0 1 t   nil))    . +empty-interval+)
    (((0 1 nil nil)    . +empty-interval+) . +empty-interval+)
    ((+empty-interval+ . (0 1 nil nil))    . +empty-interval+)
    ((+empty-interval+ . +empty-interval+) . +empty-interval+)))

(lisp-unit:define-test intersect
  (prog1 t
    (mapc
     (lambda (test-list)
       (let* ((test-intervals (car test-list))
	      (target (cdr test-list))
	      (left (car test-intervals))
	      (right (cdr test-intervals)))
	 (lisp-unit:assert-true
	  (interval=
	   (intersect
	    (if (eq left '+empty-interval+)
		+empty-interval+
		(apply #'interval left))
	    (if (eq right '+empty-interval+)
		+empty-interval+
		(apply #'interval right)))
	   (if (eq target '+empty-interval+)
	       +empty-interval+
	       (apply #'interval target))))))
     %test-intersect-intervals%)))

(defparameter %test-abs-intervals%
  '(((-3 -1 t   t  )  . 2)
    ((-3 -1 nil t  )  . 2)
    ((-3 -1 t   nil)  . 2)
    ((-3 -1 nil nil)  . 2)
    ((-2 +1 t   t  )  . 3)
    ((-2 +1 nil t  )  . 3)
    ((-2 +1 t   nil)  . 3)
    ((-2 +1 nil nil)  . 3)
    ((+0 +1 t   t  )  . 1)
    ((+0 +1 nil t  )  . 1)
    ((+0 +1 t   nil)  . 1)
    ((+0 +1 nil nil)  . 1)
    ((+1 +3 t   t  )  . 2)
    ((+1 +3 nil t  )  . 2)
    ((+1 +3 t   nil)  . 2)
    ((+1 +3 nil nil)  . 2)
    ((-1 -1 t   t  )  . 0)
    ((+0 +0 nil t  )  . 0)
    ((+1 +1 t   nil)  . 0)
    (+empty-interval+ . 0)))

(lisp-unit:define-test interval-abs
  (prog1 t
    (mapc
     (lambda (test-list)
       (let* ((test-interval (car test-list))
	      (target (cdr test-list)))
	 (lisp-unit:assert-equal
	  (abs (if (eq test-interval '+empty-interval+)
		   +empty-interval+
		   (apply #'interval test-interval)))
	  target)))
     %test-abs-intervals%)))

(defparameter %test-in-interval-intervals%
  '((((0 2 t   t  )    . 0) . t  )
    (((0 2 nil t  )    . 0) . nil)
    (((0 2 t   nil)    . 0) . t  )
    (((0 2 nil nil)    . 0) . nil)
    (((0 2 t   t  )    . 1) . t  )
    (((0 2 nil t  )    . 1) . t  )
    (((0 2 t   nil)    . 1) . t  )
    (((0 2 nil nil)    . 1) . t  )
    (((0 2 t   t  )    . 2) . t  )
    (((0 2 nil t  )    . 2) . t  )
    (((0 2 t   nil)    . 2) . nil)
    (((0 2 nil nil)    . 2) . nil)
    ((+empty-interval+ . 0) . nil)))

(lisp-unit:define-test in-interval
  (prog1 t
    (mapc
     (lambda (test-list)
       (let* ((test-cons (car test-list))
	      (test-interval (car test-cons))
	      (test-num (cdr test-cons))
	      (target (cdr test-list)))
	 (lisp-unit:assert-true
	  (not
	   (xor
	    (in-interval test-num
			 (if (eq test-interval '+empty-interval+)
			     +empty-interval+
			     (apply #'interval test-interval)))
	    target)))))
     %test-in-interval-intervals%)))