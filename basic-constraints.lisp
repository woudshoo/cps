(in-package #:cps)

(defmethod print-object ((c constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A" (variables c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All Different
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass basic-all-different (constraint)
  ((variables :reader variables :initarg :variables))
  (:default-initargs :variables (fset:empty-set))
  (:documentation "Very stupid all different, only propagates
when a domain becomes size 1."))



(defmethod propagate (solver (problem basic-problem) (constraint basic-all-different))
  (let ((vars-todo (variables constraint))
	(vars-changed (fset:empty-set)))
    (loop :until (fset:empty? vars-todo)
	  :for var = (fset:arb vars-todo)
	  :for var-domain = (domain problem var)
	  :do
	     (fset:excludef vars-todo var)
	     (case (size var-domain)
	       (0 (return-from propagate))
	       (1
		(let ((var-val (any-value var-domain)))
		  (fset:do-set (v (variables constraint))
		    (unless (eq var v)
		      (let* ((d-v (domain problem v)))
			(multiple-value-bind (domain changed)
			    (domain-without d-v var-val)
			  (when changed
			    (update-domain problem v domain)
			    (fset:includef vars-changed v)
			    (fset:includef vars-todo v))))))))))
    vars-changed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; less than than
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass basic-<=-constraint (constraint)
  ((variables :reader variables)
   (var-seq :reader var-seq :initarg :var-seq)
   (gap :reader gap :initarg :gap))
  (:default-initargs :var-seq (fset:empty-seq)
		     :step 0))


(defmethod initialize-instance :after ((constraint basic-<=-constraint)
				       &key var-seq &allow-other-keys)
  (with-slots (variables) constraint
    (setf variables (fset:convert 'fset:set var-seq))))

(defmethod print-object ((c constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A [gap: ~A]" (var-seq c) (gap c))))


(defmethod propagate (solver (problem basic-problem) (constraint basic-<=-constraint))
  (let ((vars-changed (fset:empty-set))
	(gap (gap constraint))
	(cut-off nil))
    (flet ((min-v (v)
	     (or (min-value (domain problem v)) (return-from propagate)))
	   (max-v (v)
	     (or (max-value (domain problem v)) (return-from propagate))))
      ;; walk left to right
      (fset:do-seq (v (var-seq constraint))
	(when cut-off
	  (when (< (min-v v) cut-off)
	    (update-domain problem v (domain-without-< (domain problem v) cut-off))
	    (fset:includef vars-changed v)))
	(setf cut-off (+ gap (min-v v))))
      ;; walk right to left
      (setf cut-off nil)
      (fset:do-seq (v (var-seq constraint) :from-end? t)
	(when cut-off
	  (when (> (max-v v) cut-off)
	    (update-domain problem v (domain-without-> (domain problem v) cut-off))
	    (fset:includef vars-changed v)))
	(setf cut-off (- (max-v v) gap))))
    vars-changed))
