;;; -*- Mode: Lisp; Syntax: Lisp -*-
;;; (C) 2011 Michael J. Beer

(load "cluster.lisp")

(defun create-normal-random-stream (no-samples &optional (median 0) (var nil))
  (let ((factor (sqrt (/ 12.0 no-samples)))
	(correction (* -0.5 no-samples)))
    (labels ((summarize-randoms (no)
	       (cond ((= no 0) 0.0)
		     ((= no 1) (random 1.0))
		     (t (+ (random 1.0) (summarize-randoms (1- no)))))))
      (lambda ()
	(* factor (+ correction (summarize-randoms no-samples)))))))


(defun generate-normal-random-list (no &optional (no-samples 20) (median 0) (var nil))
  (let ((next (create-normal-random-stream no-samples median var)))
    (labels ((give-me (no)
		     (cond ((< no 1) nil)
			   (t (cons (funcall next) (give-me (1- no)))))))
      (give-me no))))


(defun generate-point-list (middle-point  no &optional (var nil))
  (let ((len (length middle-point)))
    (labels ((gen-list (n)
	       (cond ((<= n 0) nil)
		     (t (cons (generate-normal-random-list len) (gen-list (1- n)))))))
      (let ((point-list (gen-list no)))
	(mapcar (lambda (x)
		  (vector-add (cons middle-point x))) point-list)))))



(defun give-me-the-flu (vec intervall no)
  (labels
      ((create-point (ints)
	 (cond ((null ints) nil)
	       (t (cons (random (car ints)) (create-point (cdr ints))))))
       (create-point-list (n)
	 (cond ((<= n 0) nil)
	       (t (cons (create-point intervall) (create-point-list (1- n)))))))
    (mapcar (lambda (x)
	      (vector-add (cons vec x)))
	    (create-point-list no))))
	      


(defun shuffle (&rest lists)
  (sort (reduce #'union lists) #'< :key (lambda (x) (random 1.0))))


(defun make-histogram (point-list &optional (no-intervalls 20))
  (let* ((max (car (give-greatest point-list (lambda (x y) (- x y)))))
	 (min (car (give-greatest point-list (lambda (x y) (- y x)))))
	 (intervall-len (/ (- max min) no-intervalls)))
    (labels ((count-within-intervall (p-list limit)
	       (cond ((not p-list) 0)
		     (t (let ((curr (car p-list)))
			  (cond ((and (>= curr limit) (< curr (+ intervall-len limit)))
				 (+ 1 (count-within-intervall (cdr p-list) limit)))
				(t (count-within-intervall (cdr p-list) limit))))))))
      (labels ((count-into-buckets (p-list limit)
		 (cond ((>= limit max) nil)
		       (t (cons
			   (cons limit (count-within-intervall p-list limit))
			   (count-into-buckets p-list (+ limit intervall-len)))))))
	(progn
	  (print min)
	  (print min)
	  (count-into-buckets point-list min))))))

			  

;; Test

;; Generate histogram of a sequence of random numbers 
(output-table (make-histogram (generate-normal-random-list 30000) 20))

;; Generate list of 2 dimensional points
(output-table (generate-point-list '(10 10 10) 10000))

(output-table (append (generate-point-list '(0 0) 3000) (generate-point-list '(20 20) 3000)))



;;; Removed stuff

;; (defun shuffle-lists (&rest lists)
;;   (let* ((remainders (length lists))
;; 	 (probability (/ 1.0 remainders))
;; 	 (current (nconc lists lists))
;; 	 (result nil))
;;     (labels ((get-next-entry ()
;; 	       (progn
;; 		 (setf current (cdr current))
;; 		 (let ((next (car current)))
;; 		   (cond ((eq 'EMPTY next)
;; 			  (get-next-entry))
;; 			 (t next))))))
;;       (loop
;; 	 (progn
;; 	   (cond ((<= remainders 0) (return result))
;; 		 ((< (random 1.0) probability)
;; 		  (let* ((curr (get-next-entry))
;; 			 (curr-point (car curr)))
;; 		    (progn
;; 		      (cond ((<= 1 (length curr))
;; 			     (progn
;; 			       (setf remainders (1- remainders))
;; 			       (setf (car current) 'EMPTY))))
;; 		      (setf result (cons curr-point result))
;; 		      (setf (car current) (remove curr-point (car current)))))))
;; 	     (print (list remainders result probability))))
;; 	result)))
