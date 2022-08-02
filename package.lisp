;;;; package.lisp

(defpackage #:cps
  (:use #:cl)
  (:import-from #:wo-util
		#:make-priority-queue #:priority-queue-push
		#:priority-queue-pop #:priority-queue-empty-p)

  (:export
   #:problem
   #:solver
   #:solve))
