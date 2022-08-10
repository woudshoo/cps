(in-package #:cps)

(defclass basic-2d-domain (basic-domain)
  ()
  (:documentation  "Stores list of points, reprented as cons cells (X . Y)."))

(defun x-value (point)  (car point))
(defun y-value (point)  (cdr point))

(defmethod min-x-value ((domain basic-2d-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'min (content domain) :key #'x-value)))

(defmethod max-x-value ((domain basic-2d-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'max (content domain) :key #'x-value)))

(defmethod min-y-value ((domain basic-2d-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'min (content domain) :key #'y-value)))

(defmethod max-y-value ((domain basic-2d-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'max (content domain) :key #'y-value)))

(make-domain-filter (domain-without-<-x basic-2d-domain value)
		    (lambda (v) (>= (x-value v) value)))

(make-domain-filter (domain-without-<-y basic-2d-domain value)
		    (lambda (v) (>= (y-value v) value)))


(make-domain-filter (domain-without->-x basic-2d-domain value)
		    (lambda (v) (<= (x-value v) value)))

(make-domain-filter (domain-without->-y basic-2d-domain value)
		    (lambda (v) (<= (y-value v) value)))
