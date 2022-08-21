(in-package #:cps)

(defclass basic-problem (problem)
  ((var-map :accessor var-map  :initarg :var-map)
   (variables :reader variables :initarg :variables)
   (constraint-map :reader constraint-map :initarg :constraint-map))
  (:default-initargs :var-map (fset:empty-map)
		     :variables (fset:empty-set)
		     :constraint-map (fset:empty-map)))

(defmethod print-object ((problem basic-problem) s)
  (print-unreadable-object (problem s :type t :identity nil)
    (format s "~A ~A"
	    (var-map problem)
	    (fset/map-values (constraint-map problem)))))


;;; Variables

(defmethod add-variable ((problem basic-problem) var domain)
  (with-slots (var-map variables) problem
    (fset:includef variables var)
    (fset:includef var-map var domain)))

;;; Constraints
(defmethod constraints ((problem basic-problem) var)
  (or (fset:lookup (constraint-map problem) var) (fset:empty-set)))

(defmethod add-constraint ((problem basic-problem) (constraint constraint))
  (with-slots (constraint-map) problem
    (fset:do-set (v (variables constraint))
      (setf constraint-map
	    (fset:with constraint-map
		       v
		       (fset:with (or (fset:lookup constraint-map v) (fset:empty-set))
				  constraint))))))

(defmethod domain ((problem basic-problem) var)
  (fset:lookup (var-map problem) var))


;;; Solver Helpers
(defmethod copy-problem ((problem basic-problem))
  (make-instance (class-of problem)
		 :var-map (var-map problem)
		 :variables (variables problem)
		 :constraint-map (constraint-map problem)))

(defmethod update-domain ((problem basic-problem) variable (new-domain domain))
  (with-slots (var-map) problem
    (fset:includef var-map variable new-domain)))

(defmethod split-domain ((prob-1 basic-problem) (prob-2 basic-problem) var)
  (let ((domains (split (domain prob-1 var))))
    (update-domain prob-1 var (first domains))
    (update-domain prob-2 var (second domains))))



