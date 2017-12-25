;;;;
;;;; Parametric package definitions.
;;;; Alex Striff.
;;;;

(defpackage #:mitty
  (:use #:common-lisp
	#:closer-mop
	#:gsll
	#:alexandria
	#:lispbuilder-sdl
	#:lispbuilder-sdl-gfx
	#:bordeaux-threads)
  (:shadowing-import-from #:closer-mop
			  #:standard-generic-function
			  #:defmethod
			  #:defgeneric)
  (:shadowing-import-from #:lispbuilder-sdl-gfx
			  #:*default-font*)
  (:shadowing-import-from #:gsll
			  #:knots
			  #:factorial
			  #:mean
			  #:variance
			  #:median
			  #:standard-deviation)
  (:shadowing-import-from #:antik
			  #:a
			  #:g
			  #:w
			  #:distance
			  #:rotate
			  #:volume)
  (:shadowing-import-from #:common-lisp
			  #:/
			  #:*
			  #:-
			  #:+))

(setf antik::*antik-user-shadow-symbols*
      '(maximizing maximize minimizing minimize multiplying antik:multiply summing
	antik:sum for time length decf incf signum round floor coerce < <= > >= = max
	min zerop minusp plusp abs exp log expt sqrt tanh cosh sinh atan acos asin tan
	cos sin aref polar-to-rectangular rectangular-to-polar acceleration
	psi knots row column sum multiply iterate))

(antik:make-user-package :mitty)

