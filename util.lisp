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
