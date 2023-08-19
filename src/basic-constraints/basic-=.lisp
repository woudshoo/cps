(in-package #:cps)

(defclass basic-= (basic-constraint)
  ()
  (:default-initargs :variables (fset:empty-set)))


(defun propagate-=-internal (problem variables key-fn)
    (labels ((content-as-set (v)
	     (fset:image key-fn
			 (fset:convert 'fset:set (domain-content problem v))))
	   (filter-seq (seq set)
	     (fset:filter (lambda (x) (fset:contains? set (funcall key-fn x)))
			  seq))
	   (new-domain (v set)
	     (let* ((domain (domain problem v))
		    (content (content domain))
		    (new-content (filter-seq content set)))
	       (unless (fset:equal? content new-content)
		 (make-instance (class-of domain) :content new-content)))))
    
    (let ((intersect (fset:reduce #'fset:intersection
				   variables
				   :key #'content-as-set))
	  (changed (fset:empty-set)))

      (fset:do-set (v variables)
	(when-let (nd (new-domain v intersect))
	  (update-domain problem v nd)
	  (fset:includef changed v)))
      changed)))

;; probably need to move the 'new-domain' to a method in 'basic-domain'
(defmethod propagate ((solver solver) (problem problem) (constraint basic-=))
  (propagate-=-internal problem (variables constraint) #'identity))


;;;;
(defclass basic-x-= (basic-constraint)  ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-x-=))
  (propagate-=-internal problem (variables constraint) #'x-value))

(defclass basic-y-= (basic-constraint)  ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-y-=))
  (propagate-=-internal problem (variables constraint) #'y-value))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass basic-x=y+z (basic-constraint)
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (z :reader z :initarg :z))
  (:default-initargs :x nil :y nil :z nil))

(defmethod initialize-instance :after ((constraint basic-x=y+z) &key x y z)
  (setf (slot-value constraint 'variables) (fset:set x y z)))


(defun domain-a*y+b*z-domains (a domain-y b domain-z)
  (let ((content (fset:empty-set)))
    (fset:do-seq (y-v domain-y)
      (fset:do-seq (z-v domain-z)
	(fset:includef content (+ (* a y-v) (* b z-v)))))
    content))

(defun domain-a*y+b*z (problem a y b z)
  "Returns the domain of a*y+b*z with a and b scalars
and y and z variables"
  (domain-a*y+b*z-domains a  (domain-content problem y) b (domain-content problem z)))

(defun restrict-domain (problem variable domain-content)
  "Update the domain to variable to the intersection
of the old domain with domain-content.

domain-content is an fset of values.

Returns a fset containing variable if the content is changed, or an
empty fset if it is not changed."
  (let* ((old-content (domain-content problem variable))
	 (new-content (fset/intersection-seq-set old-content domain-content)))

    (if (fset:equal? old-content new-content)
	(fset:set)
	(progn
	  (setf (domain-content problem variable) new-content)
	  (fset:set variable)))))

(defun propagate-x=a*y+b*z-internal (problem x a y b z)
  "Reduces domain of X based on possible values of Y and Z.
The values left in X satisfy x = a*y + b*z for some y in Y and z in Z."
  (restrict-domain problem x (domain-a*y+b*z problem a y b z)))



(defmethod propagate ((solver solver) (problem problem) (constraint basic-x=y+z))
  (let ((x (x constraint))
	(y (y constraint))
	(z (z constraint)))
    (fset:union
     (fset:union
      (propagate-x=a*y+b*z-internal problem x 1 y 1 z)
      (propagate-x=a*y+b*z-internal problem y 1 x -1 z))
     (propagate-x=a*y+b*z-internal problem z 1 x -1 y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass basic-x=abs-y-z (basic-constraint)
    ((x :reader x :initarg :x)
     (y :reader y :initarg :y)
     (z :reader z :initarg :z))
  (:documentation "Constraint x = abs(y-z)"))

(defmethod initialize-instance :after ((constraint basic-x=abs-y-z) &key x y z)
  (setf (slot-value constraint 'variables) (fset:set x y z)))

(defmethod propagate ((solver solver) (problem problem) (constraint basic-x=abs-y-z))
  (let ((x (x constraint))
	(y (y constraint))
	(z (z constraint)))
    (fset:union
     (fset:union
      (restrict-domain problem x
		       (fset:filter (lambda (v) (>= v 0))
				    (fset:union
				     (domain-a*y+b*z problem 1 y -1 z)
				     (domain-a*y+b*z problem -1 y 1 z))))
      (restrict-domain problem y
		       (fset:union
			(domain-a*y+b*z problem 1 x 1 z)
			(domain-a*y+b*z problem -1 x 1 z))))
     (restrict-domain problem z
		      (fset:union
		       (domain-a*y+b*z problem 1 y -1 x)
		       (domain-a*y+b*z problem 1 y 1 x))))))


(defclass basic-2d-d=manhatten-p (basic-constraint)
  ((d :reader d :initarg :d)
   (p :reader p :initarg :p))
  (:documentation "the 1-d variable d = abs(p-x)+abs(p-y)"))

(defmethod initialize-instance :after ((constraint basic-2d-d=manhatten-p) &key d p)
  (setf (slot-value constraint 'variables) (fset:set d p)))

(defmethod propagate ((solver solver) (problem problem) (constraint basic-2d-d=manhatten-p))
  (let* ((d (d constraint))
	 (domain-d (domain problem d))
	 (set-d (fset:set))
	 (p (p constraint))
	 (domain-p (domain problem p))
	 (set-p (fset:set)))

    (unless (and domain-d domain-p) (return-from propagate (fset:set)))

    (fset:do-seq (d-v (content domain-d))
      (fset:includef set-d d-v))
    (fset:do-seq (p-v (content domain-p))
      (fset:includef set-p p-v))
    
    (flet ((manhatten (point)
	     (+ (abs (car point)) (abs (cdr point)))))
      (fset:union
       (restrict-domain problem d
			(let ((d-v (fset:set)))
			  (fset:do-seq (point (content domain-p))
			    (fset:includef d-v (manhatten point)))
			  d-v))
       (restrict-domain problem p
			(fset:filter (lambda (point) (fset:contains? set-d (manhatten point)))
				     set-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass basic-sum-1-* (basic-ordered-constraint) ())
(defmethod propagate ((solver solver) (problem problem) (constraint basic-sum-1-*))
  (let* ((variables (var-seq constraint))
	(var-iter (fset:iterator variables))
	(min-values (list))
	(max-values (list))
	(max-sum 0) 
	(min-sum 0)
	(vars-changed (fset:set)))
    
    ;; setup and calculate values before propagation
    (fset:do-seq (v variables)
      (let* ((domain (domain problem v))
	     (min (min-value domain))
	     (max (max-value domain)))
	;; assume if min or max do not exist, the domain is empty
	(unless (and min max) (return-from propagate vars-changed))
	(assert domain)
	(assert max)
	(assert min)
	(push min min-values)
	(push max max-values)
	(incf min-sum min)
	(incf max-sum max)))
    (setf min-values (nreverse min-values))
    (setf max-values (nreverse max-values))

    ;;; first variable is special
    (when (funcall var-iter :done?) (return-from propagate vars-changed))
    (let ((first-v (funcall var-iter :get))
	  (min (pop min-values))
	  (max (pop max-values)))
      (when (> max (- max-sum max))
	(update-domain-with-fn problem first-v #'domain-without-> (- max-sum max))
	(fset:includef vars-changed first-v))
      (when (< min (- min-sum min))
	(update-domain-with-fn problem first-v #'domain-without-< (- min-sum min))
	(fset:includef vars-changed first-v))
      (decf min-sum (+ max min))
      (decf max-sum (+ max min)))

    
    (loop :until (funcall var-iter :done?)
	  :for var = (funcall var-iter :get)
	  :for min = (pop min-values)
	  :for max = (pop max-values)
	  :do
	     ;; first-max - rest-min
	     ;; first-max - (min-sum - first-min - min)
	     ;; first-max + first-min + min - min-sum
	     (when (> max (- min min-sum))
	       (update-domain-with-fn problem var #'domain-without-> (- min min-sum))
	       (fset:includef vars-changed var))


	     ;;;  first-min - rest-max
	     ;;;  first-min - (max-sum - first-max - max)
	     ;;;  first-min + first-max + max - max-sum
	     (when (< min (- max max-sum))
	       (update-domain-with-fn problem var #'domain-without-< (- max max-sum))
	       (fset:includef vars-changed var))
	  )
    vars-changed))




