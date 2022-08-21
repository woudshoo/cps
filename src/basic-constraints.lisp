(in-package #:cps)

(defmethod print-object ((c constraint) s)
  (print-unreadable-object (c s :type t :identity nil)
    (format s "~A" (variables c))))
