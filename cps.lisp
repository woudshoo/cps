;;; cps.lisp

(in-package #:cps)

(defclass solver () ()
  (:documentation "This is the base class for all CPS solvers.

Solvers should provide the following interface:

- SOLVE, for solving the problem.

For solvers to work it should implement the following supporting generic functions:

- PICK-VARIABLE, which returns the variable on which to branch
- PROPAGATE, does the constraint propagation on a set of variables.
- PROPAGATE-AND-SOLVE, which will do propagation, recursively solves the sub problems
  and return if appropriate a solved problem."))

(defclass problem () ()
  (:documentation "The base class of a problem.
In abstract, a problem consists of a set of variables with domains
and a set of constraints.

The following interface is provided to the solver:

- SOLVED-P, indicates if the problem is solved.
- SPLIT-PROBLEM, splits a problem in two on a variable.
- PROPAGATE, propagate the domain reduction of a variable.

Subclasses should implement at least:

- COPY-PROBLEM, makes an independent copy of the problem.
- SPLIT-DOMAIN, takes two prorblems and adjust the domains, used in SPLIT-PROBLEM"))

(defclass constraint () ())

(defclass domain () ())

(defgeneric solve (solver problem)
  (:documentation "Takes a SOLVER and PROBLEM and tries to solve the problem.
The return value is either a problem that is solved or nil, if no solution is found."))

(defgeneric pick-variable (solver problem)
  (:documentation "Takes a SOLVER and PROBLEM and returns a variable of the problem.
This variable should be a variable whose domain contains more than one element because
this variables domain will be split.  If this would return a variable with a domain
size of one, the solver cannot make progress."))

(defgeneric split-problem (solver problem variable)
  (:documentation "Takes a SOLVER, PROBLEM and VARIABLE.
The function will return two new problems as a cons cell: (PROB1 . PROB2).

PROB1 and PROB2 are copies of PROBLEM, but with the domain of VARIABLE reduced."))

(defgeneric propagate-and-solve (solver problem &rest vars)
  (:documentation "First propagate the domain of VARS and after that call solve the reduced problem."))

(defgeneric propagate (solver problem vars)
  (:documentation "Takes SOLVER and PROBLEM and a list VARS of variables.
It will take the list of variables as being updated, and it solve all
constraints that involve these variables.  If this updating reduces an
a domain of another variable, it will in turn solve constraints
involved in that variable etc.  until no changes in the domains of the
variables occur."))

(defgeneric solved-p (problem)
  (:documentation "Returns a genearlized boolean indicating if the PROBLEM is solved.

A problem is solved if and only if all the variables have a domain containing exactly one value."))

(defgeneric no-solution-p (problem)
  (:documentation "Returns true if there is no solution.  Note, that returning
nil does not guarentee a solution. "))

(defmethod solve ((solver solver) (problem problem))
  "Basic solver, returns a solution, no cost optimization"
  (unless (no-solution-p problem)
    (let ((var (pick-variable solver problem)))
      (destructuring-bind (prob-1 . prob-2) (split-problem solver problem var)
	(or (propagate-and-solve solver prob-1 var)
	    (propagate-and-solve solver prob-2 var))))))

(defmethod propagate-and-solve ((solver solver) (problem problem) &rest vars)
  "Returns a solved problem or nil."
  (propagate solver problem vars)
  (if (solved-p problem)
      problem
      (solve solver problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric variables (problem))
(defgeneric copy-problem (problem))
(defgeneric domain (problem variable))
(defgeneric domain-size (problem variable))
(defgeneric domain-size-1 (problem variable))
(defgeneric domain-size-0 (problem variable))
(defgeneric split-domain (problem-1 problem-2 variable))

(defmethod solved-p ((problem problem))
  (fset/every-with-carry #'domain-size-1 problem (variables problem)))

(defmethod no-solution-p ((problem problem))
  (fset/some-with-carry #'domain-size-0 problem (variables problem)))

(defmethod pick-variable ((solver solver) (problem problem))
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

(defmethod split-problem ((solver solver) (problem problem) (var t))
  (let ((prob-1 (copy-problem problem))
	(prob-2 (copy-problem problem)))
    (split-domain prob-1 prob-2 var)
    (cons prob-1 prob-2)))

(defmethod domain (problem variable)
  "Hm, is this really needed?"
  (error "Need concrete sub class"))

(defmethod domain-size (problem variable)
  (error "Need concrete sub class"))

(defmethod domain-size-1 (problem variable)
  "Default implementation, can be improved"
  (= 1 (domain-size problem variable)))

(defmethod domain-size-0 (problem variable)
  "Default implementation, can be improved"
  (= 0 (domain-size problem variable)))

(defmethod copy-problem ((problem problem))
  (error "Need concrete sub class"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric constraints (problem variable)
  (:documentation "Returns constraints for VARIABLE.
If VARIABLE is nil, return all the constraints of the PROBLEM.
Returns a fset:set"))

;;; constraint methods

(defmethod variables ((constraint constraint))
  "Variables used by the constraint."
  (fset:empty-set))

(defmethod propagate (solver problem (constraint constraint))
  "Solve the constraint and return the variables with modified constraints."
  (fset:empty-set))
