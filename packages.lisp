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

(antik:make-user-package :mitty)
