(in-package #:cps)

(defclass optimize-solver (solver)
  ())



(defmethod solve ((solver optimize-solver) (problem problem))
  (let ((best-score (upper-cost-bound problem))
	(work-items (make-priority-queue)))

    (flet ((add-to-work (problem)
	     (when (feasible-p problem)
	       (priority-queue-push problem (- (lower-cost-bound problem)) work-items)))

	   (propagate-new-cost-upper-bound (problem)
	     (set-cost-upper-bound problem best-score)
	     (propagate-cost problem)))

      (add-to-work problem)

      (loop :until (priority-queue-empty-p work-items)
	    :for problem-under-consideration = (priority-queue-pop work-items)
	    :when (<= (lower-cost-bound problem-under-consideration) best-score)
	      :do
	       (propagate-new-cost-upper-bound problem-under-consideration)
	       (let* ((variable (pick-variable problem-under-consideration))
		      (sub-problems (split-domain problem variable)))
		 (loop :for sub-problem :in sub-problems
		       :do
			  (propagate sub-problem variable)
			  (add-to-work sub-problem)))))))
