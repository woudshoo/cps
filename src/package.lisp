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
   #:add-1d-variable
   #:add-2d-variable
   #:add-<y-constraint
   #:add-<x-constraint
   #:basic-all-different
   #:add-constraint
   #:basic-domain
   #:basic-number-domain
   #:basic-<=
   #:basic-<=-1-*
   #:basic-<=-*-1
   #:basic-<=-1-*-1
   #:basic-2d-x-<=
   #:basic-2d-y-<=
   #:basic-2d-q1-<=
   #:basic-2d-x-<=-1-*
   #:basic-2d-y-<=-1-*
   #:basic-2d-x-<=-*-1
   #:basic-2d-y-<=-*-1
   #:basic-2d-x-<=-1-*-1
   #:basic-2d-y-<=-1-*-1
   #:basic-2d-q1-<=-1-*
   #:basic-2d-q1-<=-*-1
   #:basic-2d-q1-<=-1-*-1
   #:basic-=
   #:basic-x-=
   #:basic-y-=
   #:add-variable
   #:basic-2d-domain
   #:basic-2d-not-q1-<=-1-*-1
   #:variables
   #:optimizing-solver
   #:max-cost
   #:cost-constraint
   #:optimizing-problem
   #:max-2d-manhatten-cost
   #:timeout
   #:sum-cost
   #:cost
   #:basic-x=y+z
   #:max-2d-x-cost
   #:max-2d-y-cost
   #:domain-content
   #:basic-x=abs-y-z
   #:no-solution-p
   #:solved-p
   #:basic-sum-1-*
   #:basic-2d-d=manhatten-p))
