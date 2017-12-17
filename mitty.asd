;;;;
;;;; The Mitty system definition.
;;;; Alex Striff.
;;;;

(defsystem "mitty"
  :description "Mitty: A blob of shapes suffering."
  :version "0.0.1"
  :author "Alex Striff"
  :licence "MIT License"
  :depends-on (#:alexandria
	       #:lispbuilder-sdl
	       #:lispbuilder-sdl-gfx
	       #:gsll
	       #:bordeaux-threads)
  :components ((:file "packages")
	       (:file "base" :depends-on ("packages"))
	       (:file "sdl2" :depends-on ("packages" "base"))))

