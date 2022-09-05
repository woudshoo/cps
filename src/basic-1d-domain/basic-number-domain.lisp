(in-package #:cps)

(defclass basic-number-domain (basic-domain)
  ())


(make-domain-filter (domain-without-< basic-number-domain value)
		    (lambda (v) (>= v value)))

(make-domain-filter (domain-without-> basic-number-domain value)
		    (lambda (v) (<= v value)))

(make-domain-filter (domain-without-<= basic-number-domain value)
		    (lambda (v) (> v value)))

(make-domain-filter (domain-without->= basic-number-domain value)
		    (lambda (v) (< v value)))

(defmethod min-value ((domain basic-number-domain))
  "Returns the smallest value of the DOMAIN.
If the domain is empty return nil."
  (unless (fset:empty? (content domain))
    (fset:reduce #'min (content domain))))

(defmethod max-value ((domain basic-number-domain))
  "Reeturns the largest value of DOMAIN.
If the domain is empty return nil."
  (unless (fset:empty? (content domain))
    (fset:reduce #'max (content domain))))


(defmethod add-1d-variable ((problem basic-problem) var &key values)
  (let ((1d-domain (make-instance 'basic-number-domain
				  :content (seq-from-list values))))
    (add-variable problem var 1d-domain)))
