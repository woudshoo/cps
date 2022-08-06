(in-package #:cps)

(defclass basic-solver (solver)
  ())

#+nil (defmethod solve ((solver basic-solver) (problem problem))
  (call-next-method))


(defmethod propagate ((solver basic-solver) problem (vars fset:set))
  "Propagates the variabes in the set."
  (let ((constraints-todo (fset:empty-set)))
    (labels ((add-var (var)
	       (fset:unionf constraints-todo (constraints problem var)))
	     (add-vars (vars)
	       (fset:do-set (v vars) (add-var v))))
      
      (add-vars vars)
      (loop :until (fset:empty? constraints-todo)
	    :for constraint = (fset:arb constraints-todo)
	    :do
	       (add-vars (or (propagate solver problem constraint)
			     (fset:empty-set)))
	       (fset:excludef constraints-todo constraint)))))

(defmethod propagate ((solver basic-solver) problem (vars list))
  (propagate solver problem (set-from-list vars)))
