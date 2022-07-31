(in-package #:cps)

(defclass basic-problem (problem)
  ((var-map :accessor var-map  :initarg :var-map :initform (list))
   (variables :reader variables :initarg :variables)))


(defmethod domain ((problem basic-problem) var)
  (getf var (var-map problem)))

(defmethod copy-problem ((problem basic-problem))
  (make-instance (class-of problem)
		 :var-map (var-map problem)
		 :variables (variables problem)))

(defmethod update-domain ((problem basic-problem) variable new-domain)
  (push new-domain (var-map problem))
  (push variable (var-map problem)))

(defmethod split-domain ((prob-1 basic-problem) (prob-2 basic-problem) var)
  (let* ((domain (domain prob-1 var))
	 (cut-off (/ (length domain) 2)))
    (update-domain prob-1 var (subseq domain 0 cut-off))
    (update-domain prob-2 var (subseq domain cut-off))))



;;;  262 707 5630  cell.
;;   262 675 6147  home.
