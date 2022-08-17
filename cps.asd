;;;; cps.asd

(asdf:defsystem #:cps
  :description "Describe cps here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:wo-util #:fset #:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "util")
               (:file "cps")
	       (:file "basic-problem")
	       (:file "basic-solver")
	       (:file "basic-domain")
	       (:file "basic-number-domain")
	       (:file "z-order")
	       (:file "basic-2d-domain")
	       (:file "basic-constraints")
	       (:file "basic-all-different")
	       (:file "basic-<=")))
