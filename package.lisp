;;;; package.lisp

(defpackage #:cps
  (:use #:cl)
  (:import-from #:wo-util
		#:make-priority-queue #:priority-queue-push
		#:priority-queue-pop #:priority-queue-empty-p)
  (:import-from #:alexandria #:when-let)
  (:export
   #:problem
   #:solver
   #:solve
   #:basic-solver
   #:basic-problem
   #:add-2d-variable
   #:add-<y-constraint
   #:add-<x-constraint))
