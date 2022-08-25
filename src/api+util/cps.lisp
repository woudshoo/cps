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
  (:method-combination max-union)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric variables (problem))
(defgeneric copy-problem (problem))
(defgeneric domain (problem variable))
(defgeneric update-domain (problem variable domain))
(defgeneric domain-size (problem variable))
(defgeneric domain-size-1 (problem variable))
(defgeneric domain-size-0 (problem variable))
(defgeneric split-domain (problem-1 problem-2 variable))
(defgeneric add-variable (problem variable domain))
(defgeneric add-constrant (problem constraint))

(defmethod solved-p ((problem problem))
  (fset/every-with-carry-set #'domain-size-1 problem (variables problem)))

(defmethod no-solution-p ((problem problem))
  (fset/some-with-carry-set #'domain-size-0 problem (variables problem)))


(defmethod domain (problem variable)
  "Hm, is this really needed?"
  (error "Need concrete sub class"))

(defmethod domain-size (problem variable)
  (size (domain problem variable)))


(defmethod domain-size (problem (variables fset:set))
  (fset:reduce #'+ variables :key (lambda (v) (domain-size problem v))))




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

(defmethod propagate ((solver solver) (problem problem) (constraint constraint))
  "Solve the constraint and return the variables with modified constraints."
  (fset:empty-set))
