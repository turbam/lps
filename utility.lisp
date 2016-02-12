;;;; utility.lisp

(in-package #:lps)

(defun unit-vector (a)
  (vector-/ a
     (* (vector-length a))))

(defun vector-/ (v c)
  (mapcar (lambda (i) (/ i c)) v))

(defun vector-length (v)
  (sqrt (reduce #'+ (mapcar (lambda (i) (* i i)) v))))

(defun vector-* (v l)
  (cond ((and (listp v)
	      (atom l))
	 (mapcar (lambda (a) (* a l)) v))
	((and (listp l)
	      (atom v))
	 (mapcar (lambda (a) (* a v)) l))
	((and (listp l)
	      (listp v)
	      (= (length l) (length v)))
	 (reduce #'+ (mapcar #'* v l)))
	((and (atom l)
	      (atom v))
	 (* l v))
	(t (error "Something went wrong"))))

(defun vector-from-coordinates (a b)
  (cond ((not (= (length a)
		 (length b)))
	 (error "coordinates are not the same dimension!" a b))
	(t
	 (mapcar (lambda (i j) (- j i)) a b))))
