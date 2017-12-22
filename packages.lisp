;;;;
;;;; Parametric package definitions.
;;;; Alex Striff.
;;;;

(defpackage #:mitty
  (:use #:common-lisp
	#:gsll
	#:alexandria
	#:lispbuilder-sdl
	#:lispbuilder-sdl-gfx
	#:bordeaux-threads)
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
			  #:volume))

(antik:make-user-package :mitty)
