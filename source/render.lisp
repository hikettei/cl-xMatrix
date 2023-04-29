
(in-package :cl-xmatrix)

(defparameter *matrix-element-displaying-size* 13 ;; digits of single-float
  "Decides how long elements to be omitted. If 3, xmatrix prints 123 as it is but 1234 is displayed like: 1... (omitted)")

(defparameter *matrix-column-elements-displaying-length* 4
  "(1 2 3 4 5 6) -> (1 2 .. 5 6)")
(defparameter *matrix-columns-displaying-length* 2
  "((1 2 3) (4 5 6) (7 8 9)) -> ((1 2 3) ... (7 8 9))")

(defun trim-string (str max-length)
  "Trimming the given str within the length of max-length.
The result sequence MUST not over max-length.
... <- this part is not considered."
  (concatenate 'string
	       (subseq str 0 (min (length str) max-length))
	       (if (< max-length (length str))
		   "..."
		   "")))

(defun padding-str (str width &key (dont-fill nil))
  "Return a string whose length is equivalent to width."
  (let* ((trimmed-str (trim-string str (- width 3)))
	 (pad-size (- width (length trimmed-str))))
    (with-output-to-string (out)
      (format out "~a" trimmed-str)
      (unless dont-fill
	(loop repeat (- pad-size 3) do (format out " ")))
      out)))


(defun print-element (element &key (stream nil) (dont-fill nil))
  (format stream "~a"
	  (padding-str
	   (format nil "~a" element)
	   *matrix-element-displaying-size*
	   :dont-fill dont-fill)))

(defun visible-mref (matrix index)
  (with-pointer-barricade
    (mem-aref (matrix-vec (%copy matrix)) (matrix-dtype matrix) index)))

(defun pprint-1d-vector (stream
			 dim-indicator
			 matrix
			 &aux
			   (size (nth dim-indicator (matrix-visible-shape matrix)))
			   (array (convert-into-lisp-array matrix :freep nil)))
  ;;"Assertion Failed with matrix.dims == 1")

  (if (>= size
	  *matrix-column-elements-displaying-length*)
      (write-string (format nil "(~A ~A ~1~ ~A ~A)" ;; is it better: ~ -> ...
			    (print-element (aref array 0))
			    (print-element (aref array 1))
			    (print-element (aref array (- size 2)))
			    (print-element (aref array (1- size)) :dont-fill t))
		    stream)
      (progn
	(write-string "(" stream)
	(dotimes (i size)
	  (write-string (format nil "~A" (print-element (aref array i) :dont-fill (= i (1- size)))) stream)
	  (unless (= i (1- size))
	    (write-string " " stream)))
	(write-string ")" stream))))

(defun pprint-vector (stream
		      matrix
		      &optional
			(newline T)
			(indent-size 0)
			(dim-indicator 0))
  (declare (type matrix matrix))
  (cond
    ((= 1 (length (matrix-visible-shape matrix)))
     (pprint-1d-vector stream dim-indicator matrix))
    ((= dim-indicator (1- (length (matrix-visible-shape matrix))))
     (pprint-1d-vector stream dim-indicator matrix))
    (T
     (write-string "(" stream)
     (if (< (nth dim-indicator (matrix-visible-shape matrix))
	    *matrix-columns-displaying-length*)
	 ;; Can elements be printed at once?
	 (let ((first-dim (nth dim-indicator (matrix-visible-shape matrix)))
	       (args (loop repeat dim-indicator collect t)))
	   (dotimes (i first-dim)
	     ;; pprint(n-1) and indent
	     (let ((matrix1 (apply #'view matrix `(,@args ,i))))
	       (pprint-vector stream matrix1 newline (1+ indent-size) (1+ dim-indicator)))
	     
	     (unless (= i (1- first-dim))
	       (if newline
		   (progn
		     (write-char #\Newline stream)
		     ;; Rendering Indents
		     (dotimes (k (+ (1+ indent-size)))
		       (write-string " " stream)))
		   (write-string " " stream))))
	   (write-string ")" stream))
	 (let ((args (loop repeat dim-indicator collect t)))
	   (labels ((render-column (line do-newline)
		      (pprint-vector stream line newline (1+ indent-size) (1+ dim-indicator))
		      (if do-newline
			  (if newline
			      (dotimes (k (1+ indent-size))
				(write-string " " stream))))))
	     ;; Displays first and last vector

	     ;; First vector
	     (let ((matrix1 (apply #'view matrix `(,@args 0))))
	       (render-column matrix1 T))

	     ;; Newline
	     (if newline
		 (progn
		   (write-char #\newline stream)
		   ;; Fix: Indent collapses
		   (dotimes (_ (+ indent-size *matrix-element-displaying-size*))
		     (write-string " " stream))
		   ;; (write-string (format nil "(x~a)" (nth dim-indicator (matrix-visible-shape matrix))) stream)
		   (write-string "..." stream)
		   (write-char #\newline stream)
		   (dotimes (k (1+ indent-size))
		     (write-string " " stream))))

	     ;; Last Vector
	     (let ((matrix1 (apply #'view matrix `(,@args ,(1- (nth dim-indicator (matrix-visible-shape matrix)))))))
	       (render-column
		matrix1
		NIL))
	     (write-string ")" stream)))))))

(defun render-matrix (matrix &key (indent 0))
  "Renders :vec parts"
  (with-pointer-barricade
    (with-output-to-string (out)
      (let ((*matrix-element-displaying-size*
	      (+ 3 (loop for i fixnum upfrom 0 below (apply #'* (matrix-visible-shape matrix))
			 maximize (length (format nil "~a" (1d-mat-aref matrix i)))))))
	(pprint-vector out (%copy matrix) t indent)
	out))))
