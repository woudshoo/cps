(in-package #:cps)

(defclass basic-constraint (constraint)
  ((variables :reader variables :initarg :variables))
  (:default-initargs :variables (fset:empty-set)))

(defmethod print-object ((c constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A" (variables c))))
