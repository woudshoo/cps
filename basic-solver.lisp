(in-package #:cps)

(defclass basic-solver (solver)
  ())

#+nil (defmethod solve ((solver basic-solver) (problem problem))
  (call-next-method))

(defmethod propagate ((solver basic-solver) problem (vars list))
  (let ((constraints-todo (fset:empty-set)))
    (flet ((add-var (var)
	     (setf constraints-todo (fset:union constraints-todo (constraints problem var)))))
      
      (mapc #'add-var vars)
      (loop :until (fset:empty? constraints-todo)
	    :for constraint = (fset:arb constraints-todo)
	    :do
	       (mapc #'add-var  (propagate solver problem constraint))
	       (setf constraints-todo (fset:less constraints-todo constraint))))))
