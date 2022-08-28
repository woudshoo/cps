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
   #:add-1d-variable))
