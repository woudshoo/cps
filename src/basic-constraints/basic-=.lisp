(in-package #:cps)

(defclass basic-= (basic-constraint)
  ()
  (:default-initargs :variables (fset:empty-set)))


(defun propagate-=-internal (problem variables key-fn)
    (labels ((content-as-set (v)
	     (fset:image key-fn
			 (fset:convert 'fset:set (content (domain problem v)))))
	   (filter-seq (seq set)
	     (fset:filter (lambda (x) (fset:contains? set (funcall key-fn x)))
			  seq))
	   (new-domain (v set)
	     (let* ((domain (domain problem v))
		    (content (content domain))
		    (new-content (filter-seq content set)))
	       (unless (fset:equal? content new-content)
		 (make-instance (class-of domain) :content new-content)))))
    
    (let ((intersect (fset:reduce #'fset:intersection
				   variables
				   :key #'content-as-set))
	  (changed (fset:empty-set)))

      (fset:do-set (v variables)
	(when-let (nd (new-domain v intersect))
	  (update-domain problem v nd)
	  (fset:includef changed v)))
      changed)))

;; probably need to move the 'new-domain' to a method in 'basic-domain'
(defmethod propagate ((solver solver) (problem problem) (constraint basic-=))
  (propagate-=-internal problem (variables constraint) #'identity))


;;;;
(defclass basic-x-= (basic-constraint)  ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-x-=))
  (propagate-=-internal problem (variables constraint) #'x-value))

(defclass basic-y-= (basic-constraint)  ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-y-=))
  (propagate-=-internal problem (variables constraint) #'y-value))

