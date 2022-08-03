(in-package #:cps)

(defclass basic-solver (solver)
  ())

#+nil (defmethod solve ((solver basic-solver) (problem problem))
  (call-next-method))

(defmethod propagate ((solver basic-solver) problem (vars list))
  (let ((constraints-todo (fset:empty-set)))
    (flet ((add-var (var)
	     (fset:unionf constraints-todo (constraints problem var))))
      
      (mapc #'add-var vars)
      (loop :until (fset:empty? constraints-todo)
	    :for constraint = (fset:arb constraints-todo)
	    :do
	       (mapc #'add-var  (propagate solver problem constraint))
	       (fset:excludef constraints-todo constraint)))))
