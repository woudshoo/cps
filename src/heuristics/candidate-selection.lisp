(in-package #:cps)



(defmethod candidate-potential ((solver basic-solver) (problem problem) cost-constraint)
  (declare (ignore solver cost-constraint))
  (- (domain-size problem (variables problem))))


(defmethod candidate-potential ((solver optimizing-solver) (problem problem) (constraint max-cost))
  (fset:reduce #'+ (variables constraint) :key
	       (lambda (v)
		 (max-value (domain problem v)))))


(defmethod candidate-potential ((solver optimizing-solver) (problem problem) (constraint max-2d-manhatten-cost))
  (- (+
      (fset:reduce #'+ (variables constraint)
		   :key (lambda (v)
			  (max-x-value (domain problem v)))
		   :initial-value 0)
      (fset:reduce #'+ (variables constraint)
		   :key (lambda (v)
			  (max-y-value (domain problem v)))
		   :initial-value 0))))
