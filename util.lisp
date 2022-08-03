(in-package #:cps)


(defun find-min (list &key (value #'identity))
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
