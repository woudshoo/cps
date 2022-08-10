(in-package #:cps)

(defclass basic-solver (solver)
  ()
  (:documentation "Simple solver, returns a feadible solution and
does not do any cost optimization.

It requires that the problem to be solved supports the following:

- COPY-PROBLEM
- VARIABLES
- NO-SOLUTION-P
- SOLVED-P
- DOMAIN-SIZE
- SPLIT-DOMAIN"))

;;; Propagation
(defmethod propagate ((solver basic-solver) (problem problem) (vars fset:set))
  "Propagates the variabes in the set."
  (let ((constraints-todo (fset:empty-set)))
    (labels ((add-var (var)
	       (fset:unionf constraints-todo (constraints problem var)))
	     (add-vars (vars)
	       (fset:do-set (v vars) (add-var v))))
      
      (add-vars vars)
      (loop :until (fset:empty? constraints-todo)
	    :for constraint = (fset:arb constraints-todo)
	    :do
	       (add-vars (propagate solver problem constraint))
	       (fset:excludef constraints-todo constraint))))
  (fset:empty-set))

(defmethod propagate ((solver basic-solver) (problem problem) (vars list))
  (propagate solver problem (set-from-list vars)))

(defmethod propagate ((solver basic-solver) (problem problem) (var symbol))
  (propagate solver problem (fset:set var)))


;;;; Picking Branch variable
(defmethod pick-variable ((solver basic-solver) (problem problem))
  (loop :with vars = (variables problem)
	:with result-var = nil
	:with result-ds  = nil
	:until (fset:empty? vars)
	:for var = (fset:arb vars)
	:for ds  = (domain-size problem var)
	:do
	   (fset:excludef vars var)
	   (when (and (> ds 1)
		      (or (not result-ds) (> result-ds ds)))
	     (setf result-ds ds)
	     (setf result-var var))
	:finally (return result-var)))

(defmethod split-problem ((solver basic-solver) (problem problem) (var t))
  (let ((prob-1 (copy-problem problem))
	(prob-2 (copy-problem problem)))
    (split-domain prob-1 prob-2 var)
    (cons prob-1 prob-2)))

;;; Solving

(defmethod solve ((solver basic-solver) (problem problem))
  (let ((candidates (make-priority-queue))
	(count 0))
    (flet ((add-candidate (problem variables)
	     (incf count)
	     (propagate solver problem variables)
	     (unless (no-solution-p problem)
	       (priority-queue-push problem (- (domain-size problem (variables problem))) candidates))))
      
      (setf problem (copy-problem problem))
      (add-candidate problem (variables problem))
      
      (loop :until (priority-queue-empty-p candidates)
	    :for candidate = (priority-queue-pop candidates)
	    :do
	       (cond
		 ((solved-p candidate)      (return-from solve (values  candidate count)))
		; ((no-solution-p candidate) nil)
		 (t (let* ((var (pick-variable solver candidate))
			   (sub (split-problem solver candidate var)))
		      (add-candidate (car sub) var)
		      (add-candidate (cdr sub) var))))))
    (values nil count)))
