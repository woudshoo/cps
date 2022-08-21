;;;; package.lisp

(defpackage #:cps
  (:use #:cl)
  (:import-from #:wo-util
		#:make-priority-queue #:priority-queue-push
		#:priority-queue-pop #:priority-queue-empty-p)
  (:import-from #:alexandria #:when-let)
  (:export
   #:problem
   #:basic-problem
   #:solver
   #:basic-solver
   #:solve
   #:add-2d-variable
   #:add-<y-constraint
   #:add-<x-constraint
   #:basic-2d-<=-q1-constraint
   #:basic-all-different
   #:add-constraint
   #:basic-2d-range-constraint
   #:basic-2d-rest-<=-q1-1-constraint
   #:basic-2d-1-<=-q1-rest-constraint
   #:basic-2d-range-y-constraint
   #:basic-2d-range-x-constraint
   #:basic-2d-rest-<=-1-y-constraint
   #:basic-2d-rest-<=-1-x-constraint
   #:basic-2d-1-<=-rest-y-constraint
   #:basic-2d-1-<=-rest-x-constraint
   #:basic-2d-<=-y-constraint
   #:basic-2d-<=-x-constraint
   #:basic-rest-<=-1-constraint
   #:basic-1-<=-rest-constraint
   #:basic-<=-constraint
   #:basic-domain
   #:basic-number-domain))
