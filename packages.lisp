;;;;
;;;; Parametric package definitions.
;;;; Alex Striff.
;;;;

(defpackage #:mitty
  (:use #:common-lisp
	#:alexandria
	#:lispbuilder-sdl
	#:lispbuilder-sdl-gfx
	#:bordeaux-threads
	#:gsll)
  (:shadowing-import-from #:lispbuilder-sdl-gfx
			  #:*default-font*)
  (:shadowing-import-from #:gsll
			  #:knots
			  #:factorial
			  #:mean
			  #:variance
			  #:median
			  #:standard-deviation))

