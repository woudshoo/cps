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


(defmethod add-2d-variable ((problem basic-problem) var &key max-x max-y)
    (let ((2d-domain (make-instance 'basic-2d-domain
				  :content
				  (seq-from-list
				   (sort 
				    (loop :for x :upto max-x
					  :append (loop :for y :upto max-y
							:collect (cons x y)))
				    (lambda (a b) (< (z-order-from-xy (car a) (cdr a))
						     (z-order-from-xy (cdr a) (cdr b)))))))))
    (add-variable problem var 2d-domain)))

(defmethod add-<x-constraint ((problem basic-problem) var-a var-b)
  (add-constraint problem (make-instance 'basic-2d-x-<=-x
					 :gap 1
					 :var-seq (seq-from-list (list var-a var-b)))))

(defmethod add-<y-constraint ((problem basic-problem) var-a var-b)
  (add-constraint problem (make-instance 'basic-2d-y-<=
					 :gap 1
					 :var-seq (seq-from-list (list var-a var-b)))))
