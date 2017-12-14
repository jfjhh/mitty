;;;;
;;;; The Mitty system definition.
;;;; Alex Striff.
;;;;

(defsystem "mitty"
  :description "Mitty: A blob of shapes suffering."
  :version "0.0.1"
  :author "Alex Striff"
  :licence "MIT License"
  :depends-on ("alexandria" "lispbuilder-sdl" "gsll" "bordeaux-threads")
  :components ((:file "packages")
	       (:file "base" :depends-on ("packages"))
	       (:file "vector" :depends-on ("packages"))
	       (:file "sdl2" :depends-on ("packages"))
	       (:file "particle" :depends-on ("packages"))
	       (:file "screen" :depends-on ("packages"))))

