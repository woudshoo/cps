(in-package #:cps)



(defun set-from-list (list)
  (let ((result (fset:empty-set)))
    (loop :for e :in list :do
      (fset:includef result e))
    result))

(defun seq-from-list (list)
  (let ((result (fset:empty-seq)))
    (loop :for e :in list :do
      (fset:push-last result e))
    result))


(defun fset/intersection-seq-set (seq set)
  (fset:filter (lambda (x) (fset:contains? set x))
	       seq))

(defun fset/some-with-carry-set (predicate carry set)
  (fset:do-set (v set)
    (when-let (result (funcall predicate carry v))
      (return-from fset/some-with-carry-set result))))



(defun fset/every-with-carry-set (predicate carry set)
  (fset:do-set (v set)
    (unless (funcall predicate carry v)
      (return-from fset/every-with-carry-set)))
  t)




(defun fset/map-values (map)
  (fset:reduce (lambda (r k v) (declare (ignore k)) (fset:union r v))
	       map
	       :initial-value (fset:empty-set)))




(defmacro fset/do-set ((var set initial-content) &body body)
  "Iterates over the working set SET by binding VAR
to each element of SET.   The SET is initialized with INITIAL-CONTENT.
During iteration the SET can be modified.

The modification is the whole point of this macro."
  `(let ((,set ,initial-content)
	 ,var)
       (tagbody
	loop
	  (when (fset:empty? ,set)
	    (go end))
	  (setf ,var (fset:arb ,set))
	  (fset:excludef ,set ,var)
	  ,@body
	  (go loop)
	  end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-method-combination max-union ()
  ((methods () :required t))  ;; Do we need required, we could just return empty if there is no method??


  (if (rest methods)
      (flet ((wrap-call (m) `(let ((r (call-method ,m)))
			       (cond
				((eq r nil)) ; do nothing
				((fset:set? r)
				 (setf work (fset:union work r)))

				(t (error "Result for combination should be an fset:set"))))))
	
	`(let ((result (fset:empty-set))
	       work)
	   (tagbody
	    start
	      (setf work (fset:empty-set))
	      ,@ (mapcar #'wrap-call methods)
	      (when (fset:empty? work) (go end))
	      (setf result (fset:union result work))
	      (go start)
	    end)
	   result))

      ;; single case
      `(call-method ,(first methods))))



