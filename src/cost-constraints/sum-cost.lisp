(in-package #:cps)


(defclass sum-cost (basic-cost-constraint)
  ())

(defmethod propagate ((solver solver) (problem problem) (constraint sum-cost))
  (let ((cost (max-cost constraint)))
    (unless cost (return-from propagate (fset:empty-set)))
    ;; Hm, what is next?


    ))
