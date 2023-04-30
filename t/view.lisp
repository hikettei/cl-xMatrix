
(in-package :cl-xmatrix-test)

;; Testing View APIs (Handling multiple dimensions)

(in-suite :test)

(defparameter *size* 10)
(defparameter *testing-dtype* :float)

(defun initialize-mat (dims dtype)
  (let ((matrix (matrix (loop repeat dims collect *size*) :dtype dtype)))
    (%index matrix #'(lambda (x) (if (eql dtype :float)
				     (+ 0.0 x)
				     x)))
    matrix))

(defun compute-answer (&rest subscripts)
  (loop with stride = 1
	for i in (reverse subscripts)
	sum (prog1
		(* i stride)
	      (setq stride (* stride *size*)))))


(defun test-view (matrix subscripts total)
  (let ((view (apply #'view matrix subscripts)))
    (unless (= (round (%sumup view)) (round total))
      (error "The Result Didn't Match: (view matrix ~a). matrix=~a.~%~% sum=~a" subscripts matrix (%sumup view)))))

(defun testing-in-2d (dtype)
  (let ((matrix (initialize-mat 2 dtype))
	(*testing-dtype* dtype))

    ;; fixnum + t
    ;; fixnum/fixnum
    (test-view matrix (list 0 t) 45)
    (test-view matrix (list 0 0) 0)

    ;; range + t
    (test-view matrix (list `(1 3) t) 390)
    (test-view matrix (list `(0 3) t) 435)
    
    ;; range + range
    (test-view matrix (list `(0 3) `(0 3)) 99)
    (test-view matrix (list `(1 3) `(1 3)) 66)

    ;; fixnum + range
    (test-view matrix (list t `(1 3)) 930)


    ;; matrix -> view -> view

    (with-view (m* matrix `(1 8) `(1 8))
      (with-view (m* m* `(1 3) `(1 3))
	(test-view m* `(t t) 110)))
    
    ;; indices (List)

    ;; Note:: Optimize :indices/:tflist.

    (test-view matrix `((:indices 0 1 2)) 435)
    (test-view matrix `((:indices 0 1 2) 0) 30)

    ;; indices (Matrix)
    
    ;; tflist (List)
    
    (test-view matrix `((:tflist t t t)) 435)

    ;; tflist (Matrix)
    
    ;; by -1


    ;; reverse


    ;; function
    (free-mat matrix)
    t))


(test float-view-test-2d
  (is (testing-in-2d :float)))


(test uint16-view-test-2d
  (is (testing-in-2d :uint16)))


(test uint8-view-test-2d
  (is (testing-in-2d :uint8)))


;; Add: 3D/4D
;; Add: Broadcasts
;; Add:

