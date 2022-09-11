;;;; cps.asd

(asdf:defsystem #:cps
  :description "Describe cps here"
  :author "Wim Oudshoorn <woudshoo@xs4all.nl>"
  :license  "LLGPL"
  :version "0.0.1"
  :depends-on (#:wo-util #:fset #:alexandria)
  :pathname "src/"
  :components ((:file "package")
	       
	       (:module "api+util"
		:components ((:file "util")
			     (:file "cps" :depends-on ("util")))
		:depends-on ("package"))
	       
	       (:file "basic-problem" :depends-on ("api+util" "package"))
	       (:file "optimizing-problem" :depends-on ("api+util" "package" "basic-problem"))
	       (:file "basic-solver" :depends-on ("api+util" "package"))
	       (:file "basic-domain" :depends-on ("api+util" "package"))
	       (:file "basic-constraint" :depends-on ("api+util" "package"))
	       
	       (:file "optimizing-solver" :depends-on ("api+util" "package"))

	       (:module "basic-1d-domain"
		:components ((:file "basic-number-domain"))
		:depends-on ("package" "api+util" "basic-domain"))

	       (:module "basic-2d-domain"
		:components ((:file "z-order")
			     (:file "basic-2d-domain" :depends-on ("z-order")))
		:depends-on ("package" "api+util" "basic-domain"))

	       (:module "basic-constraints"
		:components ((:file "basic-constraints" )
			     (:file "basic-all-different" :depends-on ("basic-constraints"))
			     (:file "basic-<=")
			     (:file "basic-=" :depends-on ("basic-constraints")))
		:depends-on ("package" "api+util" "basic-constraint" "basic-1d-domain" "basic-2d-domain"))
	       
	       (:module "cost-constraints"
		:components ((:file "basic-cost-constraint")
			     (:file "sum-cost" :depends-on ("basic-cost-constraint"))
			     (:file "max-cost" :depends-on ("basic-cost-constraint")))
		:depends-on ("package" "basic-constraint" "api+util" "basic-domain" ))

	       (:module "heuristics"
		:components ((:file "candidate-selection"))
		:depends-on ("package" "api+util" "cost-constraints" "basic-solver" "optimizing-solver")))
  
  :in-order-to ((test-op (test-op :cps/test))))


(asdf:defsystem "cps/test"
  :depends-on (:cps :fiveam)
  :pathname "t/"
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :5am :run! :cps)))
