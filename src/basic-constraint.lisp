(in-package #:cps)

(defmethod add-constraint ((problem problem) (name symbol) &rest rest)
  (add-constraint problem
		  (apply #'make-instance name rest)))
