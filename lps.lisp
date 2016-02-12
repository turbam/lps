;;;; lps.lisp

(in-package #:lps)

;;; "lps" goes here. Hacks and glory await!

(defparameter *mass* 1)

;;car is coordinate, cdr is velocity
(defparameter *particle-list* '(((1) . 0) ((0) . 0) ((-0.5) . 0)))

(defparameter *sigma* 1.2)
(defparameter *epsilon* 0.1)

(defun lennard-jones-pot (epsilon sigma r)
  (cond ((zerop r) 0)
	(t
	 (* epsilon
	    (- (expt (/ sigma r) 12)
	       (expt (/ sigma r) 6))))))	       

(defun velocity (energy mass)
  (cond ((< energy 0) (error "Energy cannot be negative!"))
	(t (sqrt (/ (* 2 energy)
		    mass)))))

(defun get-velocity (particle other-particles)
  (loop for i in other-particles
     for r = (vector-length (vector-from-coordinates (car particle) (car i)))
     for lj-potential = (lennard-jones-pot *epsilon* *sigma* r)
     for direction = (if (< lj-potential 0)
			 (unit-vector (vector-from-coordinates (car particle) (car i)))
			 (unit-vector (vector-from-coordinates (car i) (car particle))))
     for velocity-scalar = (velocity (abs lj-potential) *mass*)
     collect (vector-* velocity-scalar direction)))
