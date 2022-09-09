(defpackage #:cps/test
  (:use #:cl #:5am #:cps)
  (:import-from #:cps
		#:set-from-list #:seq-from-list))

(in-package #:cps/test)

(def-suite :cps)
(in-suite :cps)

(defun make-basic-problem (var-with-domains)
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'basic-problem :var-map var-map :variables vars)))

(defun make-optimizing-problem (var-with-domains)
  (let ((var-map (fset:empty-map))
	(vars (fset:empty-set)))
    (loop :for (var . values) :in var-with-domains
	  :do
	     (fset:includef vars var)
	     (fset:includef var-map var (make-instance 'basic-number-domain :content (seq-from-list values))))
    (make-instance 'optimizing-problem :var-map var-map :variables vars)))

(defun add-all-different (problem vars)
  (let ((constraint (make-instance 'basic-all-different :variables (set-from-list vars))))
    (add-constraint problem constraint)))

(test test-1 ()
  (let ((p (make-basic-problem '((x 3 4 5 6) (y 2 3 4) (z 1 3 10 5 4))))
	(s (make-instance 'basic-solver)))
    (add-all-different p '(x y z))
    (add-constraint p (make-instance 'basic-<= :var-seq (seq-from-list '(y x z)) :gap 1) )
;    (format t "-- ~A~%" p)
    (setf p (solve s p))
;    (format t "-- ~A~%" p)
    (is-true p)))



(defun add-<x (p a b)
  (add-constraint p (make-instance 'basic-2d-x-<=
				   :gap 1
				   :var-seq (seq-from-list (list a b)))))

(defun add-<y (p a b)
  (add-constraint p (make-instance 'basic-2d-y-<=
				   :gap 1
				   :var-seq (seq-from-list (list a b)))))

(test test-2 
  (let ((p (make-instance 'basic-problem))
	(s (make-instance 'basic-solver)))
    (add-2d-variable p 'a :max-x 3 :max-y 3)
    (add-2d-variable p 'b :max-x 3 :max-y 3)
    (add-2d-variable p 'c :max-x 3 :max-y 3)
    (add-2d-variable p 'd :max-x 4 :max-y 3)
    (add-all-different p '(a b c d))
    (add-constraint p (make-instance 'basic-2d-x-<=-1-*-1 :var-seq '(a b c d) :gap 1))
    (add-<x p 'a 'b)
    (add-<y p 'a 'c)
    (add-<x p 'c 'd)
    (add-<y p 'b 'd)
    (is (solve s p))))


(test test-3 ()
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
    (is (solve s p))))




(test basic-= ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-= :variables (fset:set 'a 'b 'c)))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2))
    (let ((changed (cps::propagate s p c)))
      (is (fset:equal? changed (fset:set 'a 'b))))
    (is (fset:equal? (cps::content (cps::domain p 'a)) (fset:seq 2 3)))
    (is (fset:equal? (cps::content (cps::domain p 'b)) (fset:seq 2 3)))
    (is (fset:equal? (cps::content (cps::domain p 'c)) (fset:seq 3 2)))))


(test basic-x-= ()
  (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x-= :variables (fset:set 'a 'b 'c)))
	(s (make-instance 'basic-solver)))
    (add-2d-variable p 'a :max-x 3 :max-y 1)
    (add-2d-variable p 'b :max-x 2 :max-y 5)
    (add-2d-variable p 'c :max-x 1 :max-y 3)
    (let ((changed (cps::propagate s p c)))
      (is (fset:equal? changed (fset:set 'a 'b))))

    (is (= 4 (cps::domain-size p 'a)))
    (is (= 12 (cps::domain-size p 'b)))
    (is (= 8 (cps::domain-size p 'c)))
    #+nil    (is (fset:equal? (cps::content (cps::domain p 'a)) (fset:seq 2 3)))
    #+nil    (is (fset:equal? (cps::content (cps::domain p 'b)) (fset:seq 2 3)))
    #+nil    (is (fset:equal? (cps::content (cps::domain p 'c)) (fset:seq 3 2)))))

(test basic-x=y+z-1 ()
    (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x=y+z :x 'a :y 'b :z 'c))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3 4))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2))
    (let ((changed (cps::propagate s p c)))
      (is (fset:equal? changed (fset:set 'a 'b 'c))))
    (is (fset:equal? (cps::content (cps::domain p 'a)) (fset:seq 4)))
    (is (fset:equal? (cps::content (cps::domain p 'b)) (fset:seq 2)))
    (is (fset:equal? (cps::content (cps::domain p 'c)) (fset:seq 2)))))

(test basic-x=y+z-2 ()
    (let ((p (make-instance 'basic-problem))
	(c (make-instance 'basic-x=y+z :x 'a :y 'b :z 'c))
	(s (make-instance 'basic-solver)))
    (add-1d-variable p 'a :values '(2 1 3 4 10 12))
    (add-1d-variable p 'b :values '(4 2 8 3))
    (add-1d-variable p 'c :values '(3 2 8 9))
    (let ((changed (cps::propagate s p c)))
      (is (fset:equal? changed (fset:set 'a 'c))))
    (is (fset:equal? (cps::content (cps::domain p 'a)) (fset:seq 4 10 12)))
    (is (fset:equal? (cps::content (cps::domain p 'b)) (fset:seq 4 2 8 3)))
    (is (fset:equal? (cps::content (cps::domain p 'c)) (fset:seq 2 8 9)))))

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

	(let ((changed (cps::propagate s p c)))
	  (is (fset:equal? changed (fset:set 'b))))

	(is (fset:equal? (fset:seq  '(1 . 5) '(1 . 3) '(3 . 1) '(3 . 7)
				   )
			 (cps::content (cps::domain p 'b))))))

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

	(let ((changed (cps::propagate s p c)))
	  (is (fset:equal? changed (fset:set 'b))))

	(is (fset:equal? (fset:seq '(2 . 2) '(2 . 5) '(3 . 3) '(5 . 5))
			 (cps::content (cps::domain p 'b))))))


(test optimizing-solver-1 ()
  (let ((p (make-optimizing-problem '((x 3 4 5 6) (y 2 3 4) (z 1 3 10 5 4))))
	(s (make-instance 'optimizing-solver))
	(c (make-instance 'max-cost :variables (set-from-list '(x y z)))))
    (add-all-different p '(x y z))
    (setf (cost-constraint p) c)
    (add-constraint p (make-instance 'basic-<= :var-seq (seq-from-list '(y x z)) :gap 1) )
;    (format t "-- ~A~%" p)
    (setf p (solve s p))
;    (format t "-- ~A~%" p)
    (is-true p)))


(test sum-cost ()
  (let ((p (make-optimizing-problem '((x 3 4 5 6) (y 3 4) (z 1 3 10 5 4))))
	(s (make-instance 'optimizing-solver))
	(c (make-instance 'sum-cost :variables (set-from-list '(x y z)))))
    (add-all-different p '(x y z))
    (setf (cost-constraint p) c)
    (add-constraint p (make-instance 'basic-<= :var-seq (seq-from-list '(x z)) :gap 2) )
    (setf p (solve s p))
    (is-true p)
    (is (= 12 (cost p c)))))
