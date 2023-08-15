(defpackage #:cps/test
  (:use #:cl #:5am #:cps)
  (:import-from #:cps
		#:set-from-list #:seq-from-list))

(in-package #:cps/test)

(def-suite :cps)
(in-suite :cps)

(defun make-basic-problem (var-with-domains)
  "Creates a basic-problem wich has already configured the variables with domains
from var-with-domains.

VAR-WITH-DOMAINS is an alist of of the form ((var-1 . values-1) (var-2 . values-2)...)

The variables are all BASIC-NUMBER-DOMAIN variables."
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'basic-problem :var-map var-map :variables vars)))



(defmacro solved-basic-problem ((prob &optional var-with-domains) &body body)
  "Creates a basic problem which is aggined to the var PROB.
After the body is evaluated the problem is solved and the solved problem
is returned.  The solver used is the basic-solver"
  `(let ((,prob (make-basic-problem ,var-with-domains)))
     ,@body
     (solve (make-instance 'basic-solver) ,prob)))

(defun add-all-different (problem vars)
  (add-constraint problem 'basic-all-different :variables (set-from-list vars)))

(defun test-domain-equals-p (p v &rest values)
  (is (fset:equal? (domain-content p v) (seq-from-list values))
      (format nil "Domain for ~A: ~A, Expected Domain: ~A" v (domain-content p v) (seq-from-list values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test test-_1_ ()
  (is-true
   (solved-basic-problem (p '((x 3 4 5 6) (y 2 3 4) (z 1 3 10 5 4)))
     (add-constraint p 'basic-all-different :variables (fset:set 'x 'y 'z))
     (add-constraint p 'basic-<= :var-seq (seq-from-list '(y x z)) :gap 1 ))))



(defun add-<x (p a b)
  (add-constraint p 'basic-2d-x-<=
		  :gap 1
		  :var-seq (seq-from-list (list a b))))

(defun add-<y (p a b)
  (add-constraint p 'basic-2d-y-<=
		  :gap 1
		  :var-seq (seq-from-list (list a b))))

(test test-_2_
  (is-true
   (solved-basic-problem (p)
     (add-2d-variable p 'a :max-x 3 :max-y 3)
     (add-2d-variable p 'b :max-x 3 :max-y 3)
     (add-2d-variable p 'c :max-x 3 :max-y 3)
     (add-2d-variable p 'd :max-x 4 :max-y 3)
     (add-constraint p 'basic-all-different :variables (fset:set 'a 'b 'c 'd))
     (add-constraint p 'basic-2d-x-<=-1-*-1 :var-seq (seq-from-list '(a b c d)) :gap 1)
     (add-<x p 'a 'b)
     (add-<y p 'a 'c)
     (add-<x p 'c 'd)
     (add-<y p 'b 'd))))


(test test-_3_ ()
  (is-true
   (solved-basic-problem (p)
     (add-2d-variable p "a" :max-x 3 :max-y 3)
     (add-2d-variable p "b" :max-x 3 :max-y 3)
     (add-2d-variable p "c" :max-x 3 :max-y 3)
     (add-2d-variable p "d" :max-x 2 :max-y 3)
     (add-constraint p 'basic-all-different :variables (fset:set "a" "b" "c" "d"))
     (add-<x p "a" "b")
     (add-<y p "a" "c")
     (add-<x p "c" "d")
     (add-<y p "b" "d"))))


(test basic-= ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-= :variables (fset:set 'a 'b 'c)))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2))
    (is (fset:equal? (cps::propagate s p c) (fset:set 'a 'b)))
    (test-domain-equals-p p 'a 2 3)
    (test-domain-equals-p p 'b 2 3)
    (test-domain-equals-p p 'c 3 2)))


(test basic-x-= ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x-= :variables (fset:set 'a 'b 'c)))
	(s (make-instance 'basic-solver)))
    (add-2d-variable p 'a :max-x 3 :max-y 1)
    (add-2d-variable p 'b :max-x 2 :max-y 5)
    (add-2d-variable p 'c :max-x 1 :max-y 3)
    (is (fset:equal? (cps::propagate s p c) (fset:set 'a 'b)))
    (is (= 4 (cps::domain-size p 'a)))
    (is (= 12 (cps::domain-size p 'b)))
    (is (= 8 (cps::domain-size p 'c)))))

(test basic-x=y+z-_1_ ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x=y+z :x 'a :y 'b :z 'c))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3 4))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2))
    (is (fset:equal? (cps::propagate s p c) (fset:set 'a 'b 'c)))

    (test-domain-equals-p p 'a 4)
    (test-domain-equals-p p 'b 2)
    (test-domain-equals-p p 'c 2)))

(test basic-x=y+z-_2_ ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x=y+z :x 'a :y 'b :z 'c))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3 4 10 12))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2 8 9))
    (is (fset:equal? (cps::propagate s p c) (fset:set 'a 'c)))
    (test-domain-equals-p p 'a 4 10 12)
    (test-domain-equals-p p 'b 4 2 8 3)
    (test-domain-equals-p p 'c 2 8 9)))


(test basic-sum-1-*-_1_ ()
  (let ((p (solved-basic-problem (p '((s 3 5 12) (a -100 2 4) (b -3 10 11) (c -7 12)))
	     (add-constraint p 'basic-sum-1-* :var-seq (seq-from-list '(s a b c))))))
    (is-true p)
    (test-domain-equals-p p 's 5)
    (test-domain-equals-p p 'a 2)
    (test-domain-equals-p p 'b 10)
    (test-domain-equals-p p 'c -7)))

(test basic-x=abs-y-z-_1_ ()
  (let ((p (make-instance 'basic-problem))
	(s (make-instance 'basic-solver)))
    (add-constraint p 'basic-x=abs-y-z :x 'x :y 'y :z 'z)
    (add-1d-variable p 'x :values '(-10 -8 -3 3 10 12))
    (add-1d-variable p 'y :values '(-20 -8 33 10))
    (add-1d-variable p 'z :values '(8 -7 22 -13))
    (setf p (solve s p))
    (is-true p)
    (is-true (solved-p p))
    (test-domain-equals-p p 'x 12)
    (test-domain-equals-p p 'y 10)
    (test-domain-equals-p p 'z 22)))



(test basic-2d-<=-1-* ()
  (is-true
   (solved-basic-problem (p '((ll 1 2 3 4 5)
			      (b 1 2 3 4 5)
			      (c 1 2 3 4 5)
			      (ur 1 2 3 4 5)
			      (e 1 2 3 4 5)))
     (add-constraint p 'basic-<=-1-*  :var-seq (seq-from-list (list 'll 'b 'c )) :gap 0)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'b 'c)) :gap 1)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'e 'b)) :gap 1))))

(test basic-2d-<=-*-1 ()
  (is-true
   (solved-basic-problem (p '((ll 1 2 3 4 5)
			      (b 1 2 3 4 5)
			      (c 1 2 3 4 5)
			      (ur 1 2 3 4 5)
			      (e 1 2 3 4 5)))
     (add-constraint p 'basic-<=-*-1 :var-seq (seq-from-list (list  'b 'c 'ur)) :gap 0)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'b 'c)) :gap 1)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'e 'b)) :gap 1))))

(test basic-2d-<=-1-*-1 ()
  (is-true
   (solved-basic-problem (p '((ll 1 2 3 4 5)
			      (b 1 2 3 4 5)
			      (c 1 2 3 4 5)
			      (ur 1 2 3 4 5)
			      (e 1 2 3 4 5)))
     (add-constraint p 'basic-<=-1-*-1 :var-seq (seq-from-list (list  'll 'b 'c 'ur)) :gap 0)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'b 'c)) :gap 1)
     (add-constraint p 'basic-<= :var-seq (seq-from-list (list 'e 'b)) :gap 1))))




(test q1-bug-_1_ ()
  (is-true
   (solved-basic-problem (p)
     (add-2d-variable p 'a :max-x 10 :max-y 10)
     (add-2d-variable p 'b :max-x 10 :max-y 10)
     (add-2d-variable p 'c :max-x 10 :max-y 10)
     (add-2d-variable p 'll :max-x 10 :max-y 10)
     (add-2d-variable p 'ur :max-x 10 :max-y 10)
     (add-constraint p 'basic-2d-x-<=       :var-seq (seq-from-list (list 'a 'b))  :gap 1)
     (add-constraint p 'basic-2d-x-<=       :var-seq (seq-from-list (list 'c 'a)) :gap 1)
     (add-constraint p 'basic-2d-x-<=-1-*-1 :var-seq (seq-from-list '(ll a b ur))))))


(test outside-rectangle ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-2d-not-q1-<=-1-*-1 :var-seq (fset:seq 'a 'b 'c)))
	    (s (make-instance 'basic-solver)))
	
	(add-variable p 'a (make-instance 'basic-2d-domain :content (fset:seq '(2 . 2))))
	(add-variable p 'c (make-instance 'basic-2d-domain :content (fset:seq '(5 . 5))))
	(add-variable p 'b (make-instance 'basic-2d-domain
					  :content (fset:seq '(2 . 2) '(2 . 5) '(3 . 3) '(1 . 5) '(1 . 3)
							     '(3 . 1) '(3 . 7) '(5 . 5))))
	(add-constraint p c)

	(is (fset:equal? (cps::propagate s p c) (fset:set 'b)))
	(is (fset:equal? (fset:seq  '(1 . 5) '(1 . 3) '(3 . 1) '(3 . 7))
			 (domain-content p 'b)))))

(test inside-rectangle ()
      (let ((p (make-instance 'basic-problem))
	    (c (make-instance 'basic-2d-q1-<=-1-*-1 :var-seq (fset:seq 'a 'b 'c)))
	    (s (make-instance 'basic-solver)))
	(add-variable p 'a (make-instance 'basic-2d-domain :content (fset:seq '(2 . 2))))
	(add-variable p 'c (make-instance 'basic-2d-domain :content (fset:seq '(5 . 5))))
	(add-variable p 'b (make-instance 'basic-2d-domain
					  :content (fset:seq '(2 . 2) '(2 . 5) '(3 . 3) '(1 . 5) '(1 . 3)
							     '(3 . 1) '(3 . 7) '(5 . 5))))
	(add-constraint p c)

	(is (fset:equal? (cps::propagate s p c) (fset:set 'b)))
	(is (fset:equal? (fset:seq '(2 . 2) '(2 . 5) '(3 . 3) '(5 . 5))
			 (domain-content p 'b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; optimizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-optimizing-problem (var-with-domains)
  "Same as MAKE-BASIC-PROBLEM, except the problem is a optimizing-problem"
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'optimizing-problem :var-map var-map :variables vars)))

(defmacro solved-optimizing-problem ((prob &optional var-with-domains) &body body)
  "Creates a optimizing problem which is aggined to the var PROB.
After the body is evaluated the problem is solved and the solved problem
is returned.  The solver used is the optimzing-solver"
  `(let ((,prob (make-optimizing-problem ,var-with-domains)))
     ,@body
     (solve (make-instance 'optimizing-solver) ,prob)))

(defun cost-of-problem (p)
  (cost p (cost-constraint p)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test optimizing-solver-_1_ ()
  (is-true
   (solved-optimizing-problem (p '((x 3 4 5 6) (y 2 3 4) (z 1 3 10 5 4)))
     (add-all-different p '(x y z))
     (setf (cost-constraint p)
	   (make-instance 'max-cost :variables (set-from-list '(x y z))))
     (add-constraint p 'basic-<= :var-seq (seq-from-list '(y x z)) :gap 1 ))))


(test sum-cost ()
  (let ((p (solved-optimizing-problem (p '((x 3 4 5 6) (y 3 4) (z 1 3 10 5 4)))
	     (add-all-different p '(x y z))
	     (setf (cost-constraint p) (make-instance 'sum-cost :variables (set-from-list '(x y z))))
	     (add-constraint p  'basic-<= :var-seq (seq-from-list '(x z)) :gap 2 ))))
    (is-true p)
    (is (= 12 (cost-of-problem p)))))


(test sum-cost-_2_ ()
  (let ((p (solved-optimizing-problem (p '((x 3 4 5 6) (y 3 4) (z 1 3 10 5 4) (cost 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)))
	     (add-all-different p '(x y z))
	     (setf (cost-constraint p) (make-instance 'sum-cost :variables (set-from-list '(x y z))))
	     (add-constraint p  'basic-<= :var-seq (seq-from-list '(x z)) :gap 2 ))))
    (is-true p)
    (is (= 12 (cost-of-problem p)))
    (is nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

