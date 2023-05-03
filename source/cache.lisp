
(in-package :cl-xmatrix)

;; Caching API

;; Abstract:
;; *Cache-Threads*  -> Thread1 [Weak Hash Table] (place-key -> Value) 
;;                 |-> Thread2 [Weak Hash Table] 
;;                 |-> Thread3 [Weak Hash Table]
;;
;;                ...

(defvar *cache-threads* (tg:make-weak-hash-table :weakness :key)
  "Cached object by cl-xMatrix")

(defvar *thread-cache-lock*
  (bordeaux-threads:make-lock "thread cache lock"))

(defparameter *thread-idx* nil)

(defmacro get-thread-cached-objects (&key (create-if-nil nil))
  "Find out thread n"
  `(bordeaux-threads:with-lock-held (*thread-cache-lock*)
     ,(if create-if-nil
	  `(or (gethash (bordeaux-threads:current-thread) *cache-threads*)
	       (setf (gethash (bordeaux-threads:current-thread) *cache-threads*)
		     (make-hash-table :test #'eql)))
	  `(gethash (bordeaux-threads:current-thread) *cache-threads*))))

;; bottle neck?
(defun read-thread-cached-object (place-key)
  (declare (optimize (speed 3) (safety 0))
	   (type keyword place-key))
  (let ((thread (get-thread-cached-objects :create-if-nil t)))
    (gethash place-key thread)))

(defun set-thread-cached-object (place-key value)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((thread (get-thread-cached-objects :create-if-nil t)))
    (setf (gethash place-key thread) value)))

(defun adjust (matrix shape)
  "The function adjust (dedicates to caching API) returns the original/view matrix if matrix.shape == shape, or satisfies matrix.shape[i] < shape[i], otherwise free matrix and creates new one.

Assertion: Matrix != View-Object"
  (declare (optimize (speed 3))
	   (type list shape))
  (let ((return-as-it-is (equal (shape matrix) shape)) 
	(make-view-and-return (loop for i fixnum ;; find nil = or
				    upfrom 0 below (dims matrix)
				    collect (>= ;; can create cache from matrix
					     (the fixnum (nth i (shape matrix)))
					     (the fixnum (nth i shape))))))
    ;; vandr include nil -> C
    (cond
      (return-as-it-is
       matrix)
      ((not (position-if #'null make-view-and-return))
       (apply #'view matrix
	      (map 'list #'(lambda (act-dim required-size)
			     (declare (type fixnum act-dim required-size))
			     (if (= act-dim required-size)
				 t
				 `(0 ,required-size)))
	      (shape matrix)
	      shape)))
      (T
       (free-mat matrix)
       (matrix shape :dtype (matrix-dtype matrix))))))


(defmacro with-internal-system-caching ((var place-key)
					(&key
					   (if-exists nil)
					   (otherwise nil))
					&body
					  body)
  "TODO: DOC"
  `(let ((,var (read-thread-cached-object ,place-key)))
     (macrolet ((overwrite (value)
		  `(set-thread-cached-object ,,place-key ,value)))
       (if (null ,var)
	   (setq ,var (progn ,otherwise))
	   (setq ,var (progn ,if-exists)))
       ,@body)))

(defmacro with-cache ((var dimension &key (place-key :cache) (dtype :float)) &body body)
  "Caching Matrix"
  `(with-internal-system-caching (,var ,place-key)
       (:if-exists (or (when (equal ,dimension (shape ,var))
			 ,var)
		       (progn
			 (free-mat ,var)
			 (overwrite (matrix ,dimension :dtype ,dtype))))
        :otherwise (overwrite (matrix ,dimension :dtype ,dtype)))
     ,@body))


(defmacro with-caches ((&rest forms) &body body)
  ""
  (labels ((expand-caches (binding-specs body)
	     (if (endp binding-specs)
		 `(progn ,@body)
		 `(with-cache ,(first binding-specs)
	            ,(expand-caches (cdr binding-specs) body)))))
    (expand-caches forms body)))


;; with-cache (:adjustable t)
