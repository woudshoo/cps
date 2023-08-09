(in-package #:cps)

(defclass max-cost (basic-cost-constraint)
  ())


(defmethod propagate ((solver solver) (problem problem) (constraint max-cost))
  (let ((cost (max-cost constraint))
	(vars-changed (fset:empty-set)))
    (when cost
      (fset:do-set (v (variables constraint))
	(cond
	  ((domain-size-0 problem v) (return-from propagate (fset:empty-set)))
	  ((>= (max-value (domain problem v)) cost)
	   (update-domain-with-fn problem v #'domain-without->= cost)
	   (fset:includef vars-changed v)))))
    vars-changed))


(defmethod cost ((problem problem) (constraint max-cost))
  (fset:reduce #'max (variables constraint) :key
	       (lambda (v)
		 (max-value (domain problem v)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass max-2d-x-cost (basic-cost-constraint) ())

(defmethod propagate ((solver solver) (problem problem) (constraint max-2d-x-cost))
  (let ((cost (max-cost constraint))
	(vars-changed (fset:empty-set)))
    (when cost
      (fset:do-set (v (variables constraint))
	(cond
	  ((domain-size-0 problem v) (return-from propagate (fset:empty-set)))
	  ((>= (max-x-value (domain problem v)) cost)
	   (update-domain-with-fn problem v #'domain-without->=-x cost)
	   (fset:includef vars-changed v)))))
    vars-changed))

(defmethod cost ((problem problem) (constraint max-2d-x-cost))
  (fset:reduce #'max (variables constraint) :key
	       (lambda (v)
		 (max-x-value (domain problem v)))))


(defclass max-2d-y-cost (basic-cost-constraint) ())

(defmethod propagate ((solver solver) (problem problem) (constraint max-2d-y-cost))
  (let ((cost (max-cost constraint))
	(vars-changed (fset:empty-set)))
    (when cost
      (fset:do-set (v (variables constraint))
	(cond
	  ((domain-size-0 problem v) (return-from propagate (fset:empty-set)))
	  ((>= (max-y-value (domain problem v)) cost)
	   (update-domain-with-fn problem v #'domain-without->=-y cost)
	   (fset:includef vars-changed v)))))
    vars-changed))

(defmethod cost ((problem problem) (constraint max-2d-y-cost))
  (fset:reduce #'max (variables constraint) :key
	       (lambda (v)
		 (max-y-value (domain problem v)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass max-2d-manhatten-cost (max-2d-x-cost max-2d-y-cost)
  ())



(defmethod cost ((problem problem) (constraint max-2d-manhatten-cost))
  (max
   (fset:reduce #'max (variables constraint) :key
		(lambda (v)
		  (max-x-value (domain problem v))))
   (fset:reduce #'max (variables constraint) :key
		(lambda (v)
		  (max-y-value (domain problem v))))))
