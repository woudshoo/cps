(in-package #:cps)


(defclass sum-cost (basic-cost-constraint)
  ())

(defmethod propagate ((solver solver) (problem problem) (constraint sum-cost))
  (let ((cost (max-cost constraint))
	(vars-changed (fset:empty-set)))
    (when (and cost (not (no-solution-p problem)))
      (let ((head-room (- cost (fset:reduce #'+ (variables constraint)
				       :key
				       (lambda (v) (min-value (domain problem v)))))))
	(fset:do-set (v (variables constraint))
	  (let ((cut-off (+ head-room (min-value (domain problem v)))))
	    (when (<= cut-off (max-value (domain problem v)))
	      (update-domain-with-fn problem v #'domain-without->= cut-off)
	      (fset:includef vars-changed v))))))
    vars-changed))

(defmethod cost ((problem problem) (constraint sum-cost))
  (fset:reduce #'+ (variables constraint) :key
	       (lambda (v) (max-value (domain problem v)))))
