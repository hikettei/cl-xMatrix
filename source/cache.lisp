
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

(defmacro with-internal-system-caching ((var place-key)
					(&key
					   (if-exists nil)
					   (otherwise nil))
					&body
					  body)
  `(let ((,var (read-thread-cached-object ,place-key)))
     (macrolet ((overwrite (value)
		  `(set-thread-cached-object ,,place-key ,value)))
       (if (null ,var)
	   (setq ,var (progn ,@otherwise))
	   (setq ,var (progn ,@if-exists)))
       ,@body)))

(defmacro with-internal-system-caching1 ((var place-key)
				 	 (&key
					    (if-exists nil)
					    (otherwise nil))
					 &body
					   body
					 &aux (place (intern (symbol-name place-key))))
  `(progn
     (if (boundp ',place)
	 (setq ,place nil))
     (macrolet ((overwrite (value)
		  `(setf ,,(intern (symbol-name place-key)) ,value)))
       (if (null ,var)
	   (setq ,var (progn ,@otherwise))
	   (setq ,var (progn ,@if-exists)))
       ,@body)))

(defmacro with-cache ()
  "Caching Matrix"
  )
;; with-cache (:adjustable t)
