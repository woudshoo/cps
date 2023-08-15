(in-package #:cps)

(defclass basic-constraint (constraint)
  ((variables :reader variables :initarg :variables))
  (:default-initargs :variables (fset:empty-set)))

(defmethod print-object ((c constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A" (variables c))))


;; Should this be a subclass of basic-constraint??
;; argument for, it shared the slot variables
;; argument against, conceptually this only operatores
;; on the var-seq, and the variables is just for
;; caching and satisfiying the interface.
(defclass basic-ordered-constraint (constraint)
  ((variables :reader variables)
   (var-seq :reader var-seq :initarg :var-seq)
   (gap :reader gap :initarg :gap))
  (:default-initargs :var-seq (fset:empty-seq)
		     :gap 0))

(defmethod initialize-instance :after ((constraint basic-ordered-constraint)
				       &key var-seq &allow-other-keys)
  (with-slots (variables) constraint
    (setf variables (fset:convert 'fset:set var-seq))))

(defmethod print-object ((c basic-ordered-constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A [gap: ~A]" (var-seq c) (gap c))))

