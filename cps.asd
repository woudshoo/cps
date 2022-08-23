;;;; cps.asd

(asdf:defsystem #:cps
  :description "Describe cps here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:wo-util #:fset #:alexandria)
  :pathname "src/"
  :components ((:file "package")
	       
	       (:module "api+util"
		:components ((:file "util")
			     (:file "cps" :depends-on ("util")))
		:depends-on ("package"))
	       
	       (:file "basic-problem" :depends-on ("api+util"))
	       (:file "basic-solver" :depends-on ("api+util"))
	       (:file "basic-domain" :depends-on ("api+util"))


	       (:module "basic-1d-domain"
		:components ((:file "basic-number-domain"))
		:depends-on ("package" "api+util" "basic-domain"))

	       (:module "basic-2d-domain"
		:components ((:file "z-order")
			     (:file "basic-2d-domain" :depends-on ("z-order")))
		:depends-on ("package" "api+util" "basic-domain"))

	       (:module "basic-constraints"
		:components ((:file "basic-constraints" )
			     (:file "basic-all-different")
			     (:file "basic-<="))
		:depends-on ("package" "api+util" "basic-1d-domain" "basic-2d-domain")))
  
  :in-order-to ((test-op (test-op :cps/test))))


(asdf:defsystem :cps/test
  :depends-on (:cps :fiveam)
  :pathname "t/"
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :5am :run! :cps)))
