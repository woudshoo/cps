(in-package #:cps)

(defclass basic-domain (domain)
  ((content :reader content :initarg :content))
  (:default-initargs :content (fset:empty-seq)))

(defmethod print-object ((domain basic-domain) s)
  (print-unreadable-object (domain s :type nil :identity nil)
    (format s "~A" (content domain))))

(defmethod size ((domain basic-domain))
  (fset:size (content domain)))

(defmethod split ((domain basic-domain))
  (let* ((content (content domain))
	 (cut-off (floor (fset:size content) 2)))
    (list
     (make-instance 'basic-domain :content (fset:subseq content 0 cut-off))
     (make-instance 'basic-domain :content (fset:subseq content cut-off)))))


(defmethod domain-without ((domain basic-domain) value)
  (let* ((content (content domain))
	 (position (fset:position value content)))
    (if position
	(values 
	 (make-instance 'basic-domain :content (fset:less content position))
	 t)
	(values domain nil))))


(defmethod domain-without-< ((domain basic-domain) value)
  (make-instance 'basic-domain
		 :content (fset:filter (lambda (v) (>= v value)) (content domain))))

(defmethod domain-without-> ((domain basic-domain) value)
  (make-instance 'basic-domain
		 :content (fset:filter (lambda (v) (<= v value)) (content domain))))

(defmethod any-value ((domain basic-domain))
  (fset:first (content domain)))

(defmethod min-value ((domain basic-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'min (content domain))))

(defmethod max-value ((domain basic-domain))
  (unless (fset:empty? (content domain))
    (fset:reduce #'max (content domain))))
