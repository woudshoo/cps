(in-package #:cps)


#+nil (defun find-min (list &key (value #'identity))
  "Find element in LIST that minimizes the VALUE of the element.

VALUE is a function taking one argument returning a number when applied
to the elements of LIST.

If multiple elements of LIST have the same minimum value it is not
specified which element it returns."
  (loop
    :with mv = nil
    :with me = nil
    :for e :in list
    :for v = (funcall value e)
    :when (or (not mv)
	      (< v mv))
      :do
	 (setf mv v)
	 (setf me e)
    :finally (return me)))


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



(defun fset/some-with-carry (predicate carry collection)
  (loop :until (fset:empty? collection)
	:for value = (fset:arb collection)
	:for result = (funcall predicate carry value)
	:do
	   (when result (return-from fset/some-with-carry result))
	   (fset:excludef collection value))
  nil)

(defun fset/every-with-carry (predicate carry collection)
  (loop :until (fset:empty? collection)
	:for value = (fset:arb collection)
	:for result = (funcall predicate carry value)
	:do
	   (unless result (return-from fset/every-with-carry result))
	   (fset:excludef collection value))
  t)

(defun fset/map-values (map)
  (let ((result (fset:empty-set)))
    (fset:do-map (k v map)
      (declare (ignore k))
      (fset:unionf result v))
    result))




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
