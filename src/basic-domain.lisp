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
  (let* ((class   (class-of domain))
	 (content (content domain))
	 (cut-off (floor (fset:size content) 2)))
    (list
     (make-instance class :content (fset:subseq content 0 cut-off))
     (make-instance class :content (fset:subseq content cut-off)))))


(defmethod domain-without ((domain basic-domain) value)
  "Reutnrs a domain whose content is the same as DOMAIN except
VALUE is removed.  p"
  (let* ((content (content domain))
	 (position (fset:position value content)))
    (if position
	(values 
	 (make-instance (class-of domain) :content (fset:less content position))
	 t)
	(values domain nil))))

(defmethod any-value ((domain basic-domain))
  "Returns an arbitrary value out of DOMAIN"
  (fset:first (content domain)))


(defmacro make-domain-filter ((name class value) filter)
  (let ((d (gensym)))
    `(defmethod ,name ((,d ,class) ,value)
       (make-instance (class-of ,d)
		      :content (fset:filter ,filter (content ,d))))))

