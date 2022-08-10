(in-package #:cps)


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

(defun propagate-<=-internal (problem constraint fn-min fn-max fn-without< fn-without>)
    (let ((vars-changed (fset:empty-set))
	(gap (gap constraint))
	(cut-off nil))
    (flet ((min-v (v)
	     (or (funcall fn-min (domain problem v)) (return-from propagate-<=-internal (fset:empty-set))))
	   (max-v (v)
	     (or (funcall fn-max (domain problem v)) (return-from propagate-<=-internal (fset:empty-set)))))
      ;; walk left to right
      (fset:do-seq (v (var-seq constraint))
	(when cut-off
	  (when (< (min-v v) cut-off)
	    (update-domain problem v (funcall fn-without< (domain problem v) cut-off))
	    (fset:includef vars-changed v)))
	(setf cut-off (+ gap (min-v v))))
      ;; walk right to left
      (setf cut-off nil)
      (fset:do-seq (v (var-seq constraint) :from-end? t)
	(when cut-off
	  (when (> (max-v v) cut-off)
	    (update-domain problem v (funcall fn-without> (domain problem v) cut-off))
	    (fset:includef vars-changed v)))
	(setf cut-off (- (max-v v) gap))))
    vars-changed))

(defmethod propagate ((solver solver) (problem basic-problem) (constraint basic-<=-constraint))
  (propagate-<=-internal problem constraint #'min-value #'max-value #'domain-without-< #'domain-without->))


(defclass basic-2d-<=-x-constraint (basic-<=-constraint)
  ())

(defmethod propagate ((solver solver) (problem basic-problem) (constraint basic-2d-<=-x-constraint))
  (propagate-<=-internal problem constraint
			 #'min-x-value #'max-x-value
			 #'domain-without-<-x #'domain-without->-x))
