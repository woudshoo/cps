(in-package #:cps)



(defun make-basic-problem (var-with-domains)
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'basic-problem :var-map var-map :variables vars)))

(defun add-all-different (problem vars)
  (let ((constraint (make-instance 'basic-all-different :variables (set-from-list vars))))
    (add-constraint problem constraint)))



(defun test-1 ()
  (let ((p (make-basic-problem '((x 3 4 5 6) (y 2 3 4) (z 1 3 10))))
	(s (make-instance 'basic-solver)))
    (add-all-different p '(x y z))
    (add-constraint p (make-instance 'basic-<=-constraint :var-seq (seq-from-list '(y x z)) :gap 1) )
;    (propagate s p (variables p))
    (format t "PP: ~A~%~%" p)
    p
    (solve s p)
    ))





(defun add-2d-variable (problem var max-x max-y)
  (let ((2d-domain (make-instance 'basic-2d-domain
				  :content
				  (seq-from-list
				   (loop :for x :upto max-x
					 :append (loop :for y :upto max-y
						       :collect (cons x y)))))))
    (add-variable problem var 2d-domain)))


(defun add-<x (p a b)
  (add-constraint p (make-instance 'basic-2d-<=-x-constraint
				   :gap 1
				   :var-seq (seq-from-list (list a b)))))

(defun add-<y (p a b)
  (add-constraint p (make-instance 'basic-2d-<=-y-constraint
				   :gap 1
				   :var-seq (seq-from-list (list a b)))))

(defun test-2 ()
  (let ((p (make-instance 'basic-problem))
	(s (make-instance 'basic-solver)))
    (add-2d-variable p 'a 3 3)
    (add-2d-variable p 'b 3 3)
    (add-2d-variable p 'c 3 3)
    (add-2d-variable p 'd 2 3)
    (add-all-different p '(a b c d))
    (add-<x p 'a 'b)
    (add-<y p 'a 'c)
    (add-<x p 'c 'd)
    (add-<y p 'b 'd)
    (solve s p)))
