(in-package #:cps)

(defclass basic-cost-constraint (constraint)
  ((variables :reader variables :initarg :variables)
   (max-cost  :accessor max-cost :initarg :max-cost))
  (:default-initargs :variables (fset:empty-set)
		     :max-cost nil))




