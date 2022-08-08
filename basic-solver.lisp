(in-package #:cps)

(defclass basic-solver (solver)
  ())


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
	       (add-vars (propagate solver problem constraint))
	       (fset:excludef constraints-todo constraint))))
  (fset:empty-set))

(defmethod propagate ((solver basic-solver) problem (vars list))
  (propagate solver problem (set-from-list vars)))
