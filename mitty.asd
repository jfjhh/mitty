;;;;
;;;; The Mitty system definition.
;;;; Alex Striff.
;;;;

(defsystem "mitty"
  :description "Mitty: A blob of shapes suffering."
  :version "0.0.1"
  :author "Alex Striff"
  :licence "MIT License"
  :depends-on (#:closer-mop
	       #:antik
	       #:gsll
	       #:alexandria
	       #:lispbuilder-sdl
	       #:lispbuilder-sdl-gfx
	       #:bordeaux-threads)
  :components ((:file "packages")
	       (:file "ad" :depends-on ("packages"))
	       (:file "interval" :depends-on ("packages"))
	       (:file "rlambda" :depends-on ("packages" "interval"))
	       (:file "interpolation" :depends-on ("packages" "interval"))
	       (:file "physics" :depends-on ("packages"))
	       (:file "diffgeo" :depends-on ("packages" "interval" "rlambda" "interpolation"))
	       (:file "base" :depends-on ("packages" "diffgeo" "physics"))
	       (:file "sdl" :depends-on ("packages" "base"))))
