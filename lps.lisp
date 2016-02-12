;;;; lps.lisp

(in-package #:lps)

;;; "lps" goes here. Hacks and glory await!

(defparameter *mass* 1)

;;car is coordinate, cdr is velocity
(defparameter *particle-list* '(((2)  (0))
				((0.3)  (0))
				((-2) (0))))

(defparameter *sigma* 1.2)
(defparameter *epsilon* 20)

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

(defun misc (particles)
  (loop while t do
       (let* ((old-velocities (mapcar #'cadr particles))
	      (new-vel-vectors (mapcar (lambda (i) (vector-/ i 1000000))
				       (loop for i in particles
					  collect (reduce #'vector-+ (get-velocity i (remove i particles))))))
	      (new-velocities (mapcar #'vector-+ old-velocities new-vel-vectors))
	      (old-positions (mapcar #'car particles))
	      (new-positions (mapcar #'vector-+ old-positions new-velocities))
	      (new-particle-list (mapcar (lambda (a b) (list a b)) new-positions new-velocities)))
	 (format t "~d~%~d~%~%~d~%~d~%~%~%" old-positions old-velocities new-positions new-velocities)
	 (setq particles (copy-list new-particle-list))
	 (sleep 0.01))))
