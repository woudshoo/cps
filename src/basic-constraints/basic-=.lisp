(in-package #:cps)

(defclass basic-= (basic-constraint)
  ()
  (:default-initargs :variables (fset:empty-set)))


(defun propagate-=-internal (problem variables key-fn)
    (labels ((content-as-set (v)
	     (fset:image key-fn
			 (fset:convert 'fset:set (content (domain problem v)))))
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


(defun propagate-x=a*y+b*z-internal (problem x a y b z)
  "Reduces domain of X based on possible values of Y and Z.
The values left in X satisfy x = a*y + b*z for some y in Y and z in Z."
  (let ((new-domain-content (fset:empty-set))
	(vars-changed (fset:empty-set)))
    
    (fset:do-seq (y-v (content (domain problem y)))
      (fset:do-seq (z-v (content (domain problem z)))
	(fset:includef new-domain-content (+ (* a y-v) (* b z-v)))))

    (let* ((old-content (content (domain problem x)))
	   (new-content (fset:filter (lambda (v) (fset:contains? new-domain-content v))
				     old-content)))

      (unless (fset:equal? old-content new-content)
	(update-domain problem x (make-instance (class-of (domain problem x))
						:content new-content))
	(fset:includef vars-changed x)))
    vars-changed))



(defmethod propagate ((solver solver) (problem problem) (constraint basic-x=y+z))
  (let ((x (x constraint))
	(y (y constraint))
	(z (z constraint)))
    (fset:union
     (fset:union
      (propagate-x=a*y+b*z-internal problem x 1 y 1 z)
      (propagate-x=a*y+b*z-internal problem y 1 x -1 z))
     (propagate-x=a*y+b*z-internal problem z 1 x -1 y))))

