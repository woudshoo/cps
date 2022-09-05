(in-package #:cps)

(defclass basic-cost-constraint (constraint)
  ((variables :reader variables)
   (max-cost  :accessor max-cost))
  (:default-initargs :variables (fset:empty-set)
		     :max-cost nil))


