;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Creating randomly distributed vectors
;;
;; Copyright (C) 2011, 2012  Michael J. Beer <michael.josef.beer@googlemail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)

(load "numeric.lisp")

(defpackage :distribution 
  (:use  :common-lisp :numeric)
  (:export  :create-normal-random-stream   :generate-normal-random-list 
	    :generate-point-list   :give-me-the-flu 
	    :shuffle   :make-histogram))

(in-package :distribution)

(load "cluster.lisp")

(defun calculate-upper-bound-from-variance (var)
  "Used internally to calculate the length of the intervall random numbers are taken from in order to create a normally distributed random stream with variance VAR"
  (find-zero-newton (lambda (x) (- (* 1/3 x x x) (* 1/4 x x) var)) 1))

(defun create-normal-random-stream (no-samples &optional (median 0) (var 1/12))
  "Makes a function continuously returning normally distributed random numbers with median MEDIAN and variance VAR."
  (let* ((factor (sqrt (/ 1.0 no-samples))) ;12.0 no-samples))) would yield a distribution with variance = 1 if var = 1, otherwise rubbish
	(upper-bound (calculate-upper-bound-from-variance var))
	(correction (* -0.5 upper-bound no-samples)))
    (labels ((summarize-randoms (no)
	       (cond ((= no 0) 0.0)
		     ((= no 1) (random upper-bound))
		     (t (+ (random upper-bound) (summarize-randoms (1- no)))))))
      (lambda ()
	(* factor (+ correction (summarize-randoms no-samples)))))))


(defun generate-normal-random-list (no &optional (no-samples 20) (median 0) (var 1/12))
  "Creates a list of random numbers with length NO-SAMPLES, median MEDIAN and variance VAR"
  (let ((next (create-normal-random-stream no-samples median var)))
    (labels ((give-me (no)
	       (cond ((< no 1) nil)
		     (t (cons (funcall next) (give-me (1- no)))))))
      (give-me no))))


(defun generate-point-list (middle-point  no &optional (var 1/12))
  "Generates a list of NO lists of normally distributed random numbers with variance VAR. MIDDLE-POINT dis a list of numbers. Each list has as many entries as MIDDLE-POINT, where MIDDLE-POINT is taken to be the n-dimensional median"  
  (let ((len (length middle-point)))
    (labels ((gen-list (n)
	       (cond ((<= n 0) nil)
		     (t (cons (generate-normal-random-list len 20 0 var) (gen-list (1- n)))))))
      (let ((point-list (gen-list no)))
	(mapcar (lambda (x)
		  (vector-add (cons middle-point x))) point-list)))))



(defun give-me-the-flu (vec intervall no)
  "Returnes a list of equally distributed points ( = lists). VEC is a point and intervall a vector ( = list, again). All points lie within the area with one of its corners being VEC and the other corners being defined by VEC + INTERVALL."  
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
  "Takes several point (= list) lists and shuffles them into one list randomly."
  (sort (reduce #'union lists) #'< :key (lambda (x) (random 1.0))))


(defun make-histogram (point-list &optional (no-intervalls 20))
  "Creates a histogram by taking POINT-LIST partitioning the intervall the POINT-LIST covers int NO-INTERVALLS sections and counting how many points of POINT-LIST lie within each section.  Returns a list with these summaries"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate histogram of a sequence of random numbers 
;(output-table (make-histogram (generate-normal-random-list 30000) 20))

;; Generate list of 2 dimensional points
;(output-table (generate-point-list '(10 10 10) 10000))

;(output-table (append (generate-point-list '(0 0) 3000) (generate-point-list '(20 20) 3000)))



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
