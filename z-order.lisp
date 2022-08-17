(in-package #:cps)


(defun z-order-from-xy (x y)

  (let ((result 0)
	(bit 1))
    (loop :while (or (> x 0) (> y 0))
	  :for x-bit = (logand x 1)
	  :for y-bit = (logand y 1)
	  :do
	     (when (= x-bit 1) (setf result (logior result bit)))
	     (setf bit (ash bit 1))
	     (when (= y-bit 1) (setf result (logior result bit)))
	     (setf bit (ash bit 1))
	     (setf x (ash x -1))
	     (setf y (ash y -1)))
    result))
