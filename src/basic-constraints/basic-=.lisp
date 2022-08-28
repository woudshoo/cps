(in-package #:cps)

(defclass basoic-= (basic-constraint)
  ()
  (:default-initargs :variables (fset:empty-set)))


;; probably need to move the 'new-domain' to a method in 'basic-domain'
(defmethod propagate ((solver solver) (problem problem) (constraint basic-=))
  (labels ((content-as-set (v)
	     (fset:convert 'fset:set (content (domain problem v))))
	   (filter-seq (seq set)
	     (fset:filter (lambda (x) (fset:contains? set x))
			  seq))
	   (new-domain (v set)
	     (let* ((domain (domain problem v))
		    (content (content domain))
		    (new-content (filter-seq content set)))
	       (unless (fset:equal? content new-content)
		 (make-instance (class-of domain) :content new-content)))))
    
    (let ((intersect (fset:reduce #'fset:intersection
				   (variables constraint)
				   :key #'content-as-set))
	  (changed (fset:empty-set)))

      (fset:do-set (v (variables constraint))
	(when-let (nd (new-domain v intersect))
	  (update-domain problem v nd)
	  (fset:includef changed v)))
      changed)))
