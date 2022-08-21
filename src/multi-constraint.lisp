(in-package #:cps)

(defclass multi-constraint (constraint)
  ((sub-constraints :reader sub-constraints)))


(defclass basic-2d-<=-q1-constraint (multi-constraint)
  ()
  (:default-initargs :var-seq (fset:empty-seq)))

(defmethod initialize-instance :after ((constraint basic-2d-<=-q1-constraint) &key var-seq)
  (with-slots ()))

