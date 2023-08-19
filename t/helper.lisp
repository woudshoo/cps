(in-package #:cps/test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-basic-problem (var-with-domains)
  "Creates a basic-problem wich has already configured the variables with domains
from var-with-domains.

VAR-WITH-DOMAINS is an alist of of the form ((var-1 . values-1) (var-2 . values-2)...)

The variables are all BASIC-NUMBER-DOMAIN variables."
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'basic-problem :var-map var-map :variables vars)))



(defmacro solved-basic-problem ((prob &optional var-with-domains) &body body)
  "Creates a basic problem which is aggined to the var PROB.
After the body is evaluated the problem is solved and the solved problem
is returned.  The solver used is the basic-solver"
  `(let ((,prob (make-basic-problem ,var-with-domains)))
     ,@body
     (solve (make-instance 'basic-solver) ,prob)))

(defun add-all-different (problem vars)
  (add-constraint problem 'basic-all-different :variables (set-from-list vars)))

(defun test-domain-equals-p (p v &rest values)
  (is (fset:equal? (domain-content p v) (seq-from-list values))
      (format nil "Domain for ~A: ~A, Expected Domain: ~A" v (domain-content p v) (seq-from-list values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-optimizing-problem (var-with-domains)
  "Same as MAKE-BASIC-PROBLEM, except the problem is a optimizing-problem"
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'optimizing-problem :var-map var-map :variables vars)))

(defmacro solved-optimizing-problem ((prob &optional var-with-domains) &body body)
  "Creates a optimizing problem which is aggined to the var PROB.
After the body is evaluated the problem is solved and the solved problem
is returned.  The solver used is the optimzing-solver"
  `(let ((,prob (make-optimizing-problem ,var-with-domains)))
     ,@body
     (solve (make-instance 'optimizing-solver) ,prob)))

(defun cost-of-problem (p)
  (cost p (cost-constraint p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-2d-x-< (p a b)
  (add-constraint p 'basic-2d-x-<=
		  :gap 1
		  :var-seq (seq-from-list (list a b))))

(defun add-2d-y-< (p a b)
  (add-constraint p 'basic-2d-y-<=
		  :gap 1
		  :var-seq (seq-from-list (list a b))))
