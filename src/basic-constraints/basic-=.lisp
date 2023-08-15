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


(defun domain-a*y+b*z (problem a y b z)
  "Returns the domain of a*y+b*z with a and b scalars
and y and z variables"
  (let ((content (fset:empty-set)))
    (fset:do-seq (y-v (domain-content problem y))
      (fset:do-seq (z-v (domain-content problem z))
	(fset:includef content (+ (* a y-v) (* b z-v)))))
    content))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


