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
	#:bordeaux-threads
	#:cl-speedy-queue)
  (:shadowing-import-from #:closer-mop
			  #:standard-generic-function
			  #:defmethod
			  #:defgeneric)
  (:shadowing-import-from #:lispbuilder-sdl-gfx
			  #:*default-font*)
  (:shadowing-import-from #:gsll
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
  (:shadowing-import-from #:grid
			  #:grid
			  #:aref
			  #:foreign-array
			  #:make-grid
			  #:map-grid
			  #:make-grid-sequential-elements
			  #:normalize)
  (:shadowing-import-from #:common-lisp
			  #:/
			  #:*
			  #:-
			  #:+)
  (:shadow :knots))
