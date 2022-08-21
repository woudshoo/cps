(defpackage #:cps/test
  (:use #:cl #:5am #:cps)
  (:import-from #:cps
		#:set-from-list #:seq-from-list))

(in-package #:cps/test)


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
    p
    (solve s p)))



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
    (add-2d-variable p 'a :max-x 3 :max-y 3)
    (add-2d-variable p 'b :max-x 3 :max-y 3)
    (add-2d-variable p 'c :max-x 3 :max-y 3)
    (add-2d-variable p 'd :max-x 4 :max-y 3)
    (add-all-different p '(a b c d))
    (add-constraint p (make-instance 'basic-2d-range-x-constraint :var-seq '(a b c d) :gap 1))
    (add-<x p 'a 'b)
    (add-<y p 'a 'c)
    (add-<x p 'c 'd)
    (add-<y p 'b 'd)
    (solve s p)))


(defun test-3 ()
    (let ((p (make-instance 'basic-problem))
	  (s (make-instance 'basic-solver)))
    (add-2d-variable p "a" :max-x 3 :max-y 3)
    (add-2d-variable p "b" :max-x 3 :max-y 3)
    (add-2d-variable p "c" :max-x 3 :max-y 3)
    (add-2d-variable p "d" :max-x 2 :max-y 3)
    (add-all-different p '("a" "b" "c" "d"))
    (add-<x p "a" "b")
    (add-<y p "a" "c")
    (add-<x p "c" "d")
    (add-<y p "b" "d")
      (solve s p)))




