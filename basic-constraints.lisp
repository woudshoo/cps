(in-package #:cps)



(defclass basic-all-different (constraint)
  ((variables :reader variables :initarg :variables))
  (:default-initargs :variables (fset:empty-set))
  (:documentation "Very stupid all different, only propagates
when a domain becomes size 1."))


(defmethod print-object ((c basic-all-different) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A" (variables c))))

(defmethod propagate (solver (problem basic-problem) (constraint basic-all-different))
  (let ((vars-todo (variables constraint)))
    (loop :until (fset:empty? vars-todo)
	  :for var = (fset:arb vars-todo)
	  :for var-domain = (domain problem var)
	  :do
	     (fset:excludef vars-todo var)
	     (case (size var-domain)
	       (0 (return-from propagate))
	       (1
		(let ((var-val (any-value var-domain)))
		  (fset:do-set (v (variables constraint))
		    (unless (eq var v)
		      (let* ((d-v (domain problem v)))
			(multiple-value-bind (domain changed)
			    (domain-without d-v var-val)
			  (when changed
			    (update-domain problem v domain)
			    (fset:includef vars-todo v))))))))))))
