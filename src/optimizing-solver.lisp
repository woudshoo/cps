(in-package #:cps)

(defclass optimizing-solver (basic-solver)
  ()
  (:documentation "Solver that optimizes a problem against a cost constraint."))



(defmethod solve ((solver optimizing-solver) (problem optimizing-problem))
  (let ((candidates (make-priority-queue))
	(count 0)
	(best-solution nil)
	;; the below is dodgy, we modify the actual constraint in the original problem
	(cost-constraint (cost-constraint problem)))
    (unless cost-constraint (error "Should specify cost constraint in an optimizing solver"))
    (flet ((add-candidate (problem variables)
	     (incf count)
	     (propagate solver problem
			(fset:union  (propagate solver problem cost-constraint) 	     
				     variables))
	     (unless (no-solution-p problem)
	       (priority-queue-push problem (- (domain-size problem (variables problem))) candidates))))
      
      (setf problem (copy-problem problem))
      (add-candidate problem (variables problem))
      (loop :until (priority-queue-empty-p candidates)
	    :for candidate = (priority-queue-pop candidates)
	    :do
	       (cond
		 ((solved-p candidate)
		  (setf best-solution candidate)
		  (setf (max-cost cost-constraint) (cost candidate cost-constraint))
		  #+nil (format t "NC: ~A -- S: ~A~%" (max-cost cost-constraint) best-solution))

		 (t (let* ((var (pick-variable solver candidate))
			   (sub (split-problem solver candidate var)))
		      (add-candidate (car sub) (fset:set var))
		      (add-candidate (cdr sub) (fset:set var)))))))
    (values best-solution count)))
