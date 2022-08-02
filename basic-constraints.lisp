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
	  :for domain-var = (content (domain problem var))
	  :do
	     (setf vars-todo (fset:less vars-todo var))
	     (case (fset:size domain-var)
	       (0 (return-from propagate))
	       (1
;		(format t "ARB-DOMAIN-VAR: ~A~%" domain-var)
		(let ((var-val (fset:first domain-var)))
		  (fset:do-set (v (variables constraint))
		    (unless (eq var v)
;		      (format t "Var: ~A, V: ~A~%" var v)
		      (let* ((d-v (domain problem v))
			     (p (fset:position var-val (content d-v))))
			(when p
;			  (format t "Found: ~A in ~A, vars-todo: ~A~%" var-val (content d-v) vars-todo)
			  (update-domain problem v (domain-without d-v p))
	;		  (format t "New domain: ~A~%" (content (domain problem v)))
			  (setf vars-todo (fset:with vars-todo v))))))))))))
