(in-package #:cps)

(defclass basic-all-different (basic-constraint)
  ()
  (:documentation "Very stupid all different, only propagates
when a domain becomes size 1."))


(defmethod propagate ((solver solver) (problem problem) (constraint basic-all-different))
  (let ((vars-changed (fset:empty-set)))
    (fset/do-set (var vars-todo (variables constraint))
      (let ((var-domain (domain problem var)))
	(case (size var-domain)
	  (0 (return-from propagate (fset:empty-set)))
	  (1
	   (let ((var-val (any-value var-domain)))
	     (fset:do-set (v (variables constraint))
	       (unless (eq var v)
		 (let* ((d-v (domain problem v)))
		   (multiple-value-bind (domain changed)
		       (domain-without d-v var-val)
		     (when changed
		       (update-domain problem v domain)
		       (fset:includef vars-changed v)
		       (fset:includef vars-todo v)))))))))))
    vars-changed))
