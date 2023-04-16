
(in-package :cl-xmatrix)

(defparameter *matrix-element-displaying-size* 10
  "Decides how long elements to be omitted. If 3, xmatrix prints 123 as it is but 1234 is displayed like: 1... (omitted)")


(defparameter *matrix-columns-displaying-length* 10)

(defun trim-string (str max-length)
  "Trimming the given str within the length of max-length.
The result sequence MUST not over max-length.
... <- this part is not considered."
  (concatenate 'string
	       (subseq str 0 (min (length str) max-length))
	       (if (< max-length (length str))
		   "..."
		   "")))

(defun padding-str (str width)
  "Return a string whose length is equivalent to width."
  (let* ((trimmed-str (trim-string str (- width 3)))
	 (pad-size (- width (length trimmed-str))))
    (with-output-to-string (out)
      (format out "~a" trimmed-str)
      (loop repeat pad-size do (format out " "))
      out)))


(defun print-element (element &key (stream t))
  (format stream "~a"
	  (padding-str
	   (format nil "~a" element)
	   *matrix-element-displaying-size*)))


(defun pprint-columns (matrix dim dims &aux (half-pos
					     (round (/
						     *matrix-columns-displaying-length*
						     2))))
  (let ((views (map 'list #'(lambda (x)
			      (if (< x *matrix-columns-displaying-length*)
				  `(:indices ,@(loop for i upfrom 0 below x
						     collect i))
				  `(:indices ,@(loop for i upfrom 0 below half-pos
						     collect i)
					     ,@(loop for i upfrom (- x half-pos) below half-pos
						     collect i))))
		    dims)))

    (let ((matrix (apply #'view matrix views)))
      (call-with-visible-area
       matrix
       #'(lambda (view)
	   (with-view-object (index view :absolute index1)
	     
	     ))))))

(defun render-matrix (matrix)
  "Renders :vec parts"
  (with-output-to-string (out)
    (let* ((dimensions (matrix-visible-shape matrix)))


      )))
