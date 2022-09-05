(in-package #:cps)

(defclass optimizing-problem (basic-problem)
  ((cost-constraint :initarg :cost-constraint :reader cost-constraint))
  (:default-initargs :cost-constraint nil))

(defmethod initialize-instance :after ((problem optimizing-problem) &key cost-constraint &allow-other-keys)
  (when cost-constraint
    (add-constraint problem cost-constraint)))

(defmethod (setf cost-constraint) ((new-value constraint) (problem optimizing-problem))
  (let ((old-value (cost-constraint problem)))
    (when old-value (error "Can only set cost constraint once"))
    (setf (slot-value problem 'cost-constraint) new-value)
    (add-constraint problem new-value)))

(defmethod copy-problem ((problem optimizing-problem))
  "TODO Probably need to copy the cost constraint.  However it needs to be shared?"
  (let ((result (call-next-method)))
    (when (cost-constraint problem)
      (setf (cost-constraint result) (cost-constraint problem)))
    result))
