(in-package #:cps)

;; Should this be a subclass of basic-constraint??
;; argument for, it shared the slot variables
;; argument against, conceptually this only operatores
;; on the var-seq, and the variables is just for
;; caching and satisfiying the interface.
(defclass basic-ordered-constraint (constraint)
  ((variables :reader variables)
   (var-seq :reader var-seq :initarg :var-seq)
   (gap :reader gap :initarg :gap))
  (:default-initargs :var-seq (fset:empty-seq)
		     :gap 0))

(defmethod initialize-instance :after ((constraint basic-ordered-constraint)
				       &key var-seq &allow-other-keys)
  (with-slots (variables) constraint
    (setf variables (fset:convert 'fset:set var-seq))))

(defmethod print-object ((c basic-ordered-constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A [gap: ~A]" (var-seq c) (gap c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun update-domain-with-fn (problem v fn &rest args)
  (update-domain problem v (apply fn (domain problem v) args)))

(defun propagate-<=-internal (problem constraint fn-min fn-max fn-without< fn-without>)
  "Function used to implement ordering constraint.
If the variables of the constraint are (v_1, v_2, ..., v_n) the following constraint holds:

for all i > 1:    v_{i-1} + gap <= v_i."
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
	      (update-domain-with-fn problem v fn-without< cut-off)
	      (fset:includef vars-changed v)))
	  (setf cut-off (+ gap (min-v v))))
	;; walk right to left
	(setf cut-off nil)
	(fset:do-seq (v (var-seq constraint) :from-end? t)
	  (when cut-off
	    (when (> (max-v v) cut-off)
	      (update-domain-with-fn problem v fn-without> cut-off)
	      (fset:includef vars-changed v)))
	  (setf cut-off (- (max-v v) gap))))
      vars-changed))


(defun propagate-min-internal (problem constraint fn-min fn-max fn-without< fn-without>)
  "Propages the constraint that in the var seqs = (v_1, v_2, ..., v_n) the following holds:

for all i > i :   v_1 + gap <= v_i"
  (let ((vars-changed (fset:empty-set))
	(gap (gap constraint))
	(iter (fset:iterator (var-seq constraint))))
      (flet ((min-v (v)
	       (or (funcall fn-min (domain problem v)) (return-from propagate-min-internal (fset:empty-set))))
	     (max-v (v)
	       (or (funcall fn-max (domain problem v)) (return-from propagate-min-internal (fset:empty-set)))))
	;; walk left to right
	(loop
	  :with boundary-v = (funcall iter :get)
	  :with cut-off-1 = (+  (min-v boundary-v) gap)
	  :with cut-off-2 = (+  (max-v boundary-v) gap)
	  :for v = (funcall iter :get)
	  :while v
	  :for min-v = (min-v v)
	  :for max-v = (max-v v)
	  :when (< min-v cut-off-1) :do
	    (update-domain-with-fn problem v fn-without< cut-off-1)
	    (fset:includef vars-changed v)
	  :when (> cut-off-2 max-v) :do
	    (update-domain-with-fn problem boundary-v fn-without> (- max-v gap))
	    (fset:includef vars-changed boundary-v)
	    ;; the new cut-off-2 is not exact, but better than leaving it unmodified.
	    ;; making it exact seems a waste of time
	    (setf cut-off-2 max-v)))
    vars-changed))

(defun propagate-max-internal (problem constraint fn-min fn-max fn-without< fn-without>)
  "Propages the constraint that in the var seqs = (v_1, v_2, ..., v_n) the following holds:

for all i < n :   v_i + gap <= v_n"
  (let ((vars-changed (fset:empty-set))
	(gap (gap constraint))
	(iter (fset:iterator (var-seq constraint))))
      (flet ((min-v (v)
	       (or (funcall fn-min (domain problem v)) (return-from propagate-max-internal (fset:empty-set))))
	     (max-v (v)
	       (or (funcall fn-max (domain problem v)) (return-from propagate-max-internal (fset:empty-set)))))
	;; walk left to right

	(loop
	  :with boundary-v = (fset:last (var-seq constraint))
	  :with cut-off-1 = (-  (min-v boundary-v) gap)
	  :with cut-off-2 = (-  (max-v boundary-v) gap)
	  :for v = (funcall iter :get)
	  :while v
	  :until (eql v boundary-v)
	  :for min-v = (min-v v)
	  :for max-v = (max-v v)
	  :when (> max-v cut-off-2) :do
	    (update-domain-with-fn problem v fn-without> cut-off-2)
	    (fset:includef vars-changed v)
	  :when (< cut-off-1 min-v) :do
	    (update-domain-with-fn problem boundary-v fn-without< (+ min-v gap))
	    (fset:includef vars-changed boundary-v)
	    ;; the new cut-off-2 is not exact, but better than leaving it unmodified.
	    ;; making it exact seems a waste of time
	    (setf cut-off-2 min-v)))
    vars-changed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1D ordered constraint ------------------------------------------------------------
(defclass basic-<= (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-<=))
  (propagate-<=-internal problem constraint
			 #'min-value #'max-value
			 #'domain-without-< #'domain-without->))


;;;;
(defclass basic-<=-1-* (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-<=-1-*))
  (propagate-min-internal problem constraint
			 #'min-value #'max-value
			 #'domain-without-< #'domain-without->))


(defclass basic-<=-*-1 (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-<=-*-1))
  (propagate-max-internal problem constraint
			 #'min-value #'max-value
			 #'domain-without-< #'domain-without->))


(defclass basic-<=-1-*-1 (basic-<=-1-* basic-<=-*-1) ())

;;; 2D  X constraint --------------------------------------------------------------------------------
(defclass basic-2d-x-<= (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-x-<=))
  (propagate-<=-internal problem constraint
			 #'min-x-value #'max-x-value
			 #'domain-without-<-x #'domain-without->-x))

;;; 2D Y constraint
(defclass basic-2d-y-<= (basic-ordered-constraint)  ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-y-<=))
  (propagate-<=-internal problem constraint
			 #'min-y-value #'max-y-value
			 #'domain-without-<-y #'domain-without->-y))

;;; 2D Q1 constraint
(defclass basic-2d-q1-<= (basic-2d-x-<= basic-2d-y-<=) ()
  (:documentation "A <=-q1 B is the same as a_x <= b_x and a_y <= b_y.
So basically, B is the first quadrant relative to A."))

;;; ranges

(defclass basic-2d-x-<=-1-* (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-x-<=-1-*))
  (propagate-min-internal problem constraint
			 #'min-x-value #'max-x-value
			 #'domain-without-<-x #'domain-without->-x))


(defclass basic-2d-y-<=-1-* (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-y-<=-1-*))
  (propagate-min-internal problem constraint
			 #'min-y-value #'max-y-value
			 #'domain-without-<-y #'domain-without->-y))


(defclass basic-2d-x-<=-*-1 (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-x-<=-*-1))
  (propagate-max-internal problem constraint
			 #'min-x-value #'max-x-value
			 #'domain-without-<-x #'domain-without->-x))


(defclass basic-2d-y-<=-*-1 (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-y-<=-*-1))
  (propagate-max-internal problem constraint
			 #'min-y-value #'max-y-value
			 #'domain-without-<-y #'domain-without->-y))

;;;

(defclass basic-2d-x-<=-1-*-1              (basic-2d-x-<=-1-* basic-2d-x-<=-*-1) ())
(defclass basic-2d-y-<=-1-*-1              (basic-2d-y-<=-1-* basic-2d-y-<=-*-1) ())
(defclass basic-2d-q1-<=-1-*               (basic-2d-x-<=-1-* basic-2d-y-<=-1-*) ())
(defclass basic-2d-q1-<=-*-1               (basic-2d-x-<=-*-1 basic-2d-y-<=-*-1) ())
(defclass basic-2d-q1-<=-1-*-1             (basic-2d-x-<=-1-*-1 basic-2d-y-<=-1-*-1) ())
