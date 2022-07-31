;;;; cps.lisp

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

(defmethod solve ((solver solver) (problem problem))
  "Basic solver, returns a solution, no cost optimization"
    (let ((var (pick-variable solver problem)))
      (destructuring-bind (prob-1 . prob-2) (split-problem solver problem var)
	(or (propagate-and-solve solver prob-1 var)
	    (propagate-and-solve solver prob-2 var)))))

(defmethod propagate-and-solve ((solver solver) (problem problem) &rest vars)
  "Returns a solved problem or nil."
  (when (propagate solver problem vars)
    (if (solved-p problem)
	problem
	(solve solver problem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric variables (problem))
(defgeneric copy-problem (problem))
(defgeneric domain (problem variable))
(defgeneric domain-size (problem variable))
(defgeneric domain-size-1 (problem variable))
(defgeneric split-domain (problem-1 problem-2 variable))

(defmethod solved-p ((problem problem))
  (every #'domain-size-1 (variables problem)))

(defmethod pick-variable ((solver solver) (problem problem))
  (find-min (variables problem)
	    :value (lambda (var) (domain-size problem var))))

(defmethod split-problem ((solver solver) (problem problem) (var t))
  (let ((prob-1 (copy-problem problem))
	(prob-2 (copy-problem problem)))
    (split-domain prob-1 prob-2 var)
    (cons prob-1 prob-2)))

(defmethod domain-size (problem variable)
  (length (domain problem variable)))

(defmethod domain-size-1 (problem variable)
  "Default implementation, can be improved"
  (= 1 (domain-size problem variable)))

(defmethod copy-problem ((problem problem))
  (error "Need concrete sub class"))
