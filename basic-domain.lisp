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



(defmethod domain-without ((domain basic-domain) position)
  (let* ((content (content domain)))
    (make-instance 'basic-domain :content (fset:less content position))))
