;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My Glorious Clustering - Test of package functionality of Common Lisp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :MB-SEQUENCE
  (:documentation "Provides functions for sorting streams")
  (:use :COMMON-LISP)
  (:export :MAKE-ORDERED-STREAM :COMPARE-MV-1D :COMPARE-MV-ND :REVERT-COMPARE-FUNC :give-ordered))


(in-package "MB-SEQUENCE")


(defun compare-mv-1d  (x y)
  "SEQUENCE:COMPARATOR that takes numeric types"
  (- x y))


(defmacro compare-mv-nd (n)
  "Returns a compare-func that uses the n-th element of two vectors to compare the vecto
rs"
  (let ((x (gensym)) (y (gensym)))
  `(lambda (,x ,y) (- (nth ,n ,x) (nth ,n ,y)))))


(defmacro revert-compare-func (f)
  "Returnes a compare-func that defines an order opposite to the order given by the compare-func F"
  `(lambda (x y)
     (- 0 (funcall ,f x y))))

(defmacro give-max (x y compare-func)
  "Returns the maximum of X and Y using COMPARE-FUNC for comparation"
  `(cond ((< (funcall ,compare-func ,x ,y) 0) ,y)
	 (T ,x)))


(defmacro give-min (x y compare-func)
  "Returns the minimum of X and Y using COMPARE-FUNC for comparation"
  `(cond ((< (funcall ,compare-func ,x ,y) 0) ,x)
	  (T ,y)))


(defmacro return-if-greater (x y compare-func)
  "Returnes X if X is greater as Y by comparing using COMPARE-FUNC"
  `(cond ((<= (funcall ,compare-func ,x ,y) 0) nil)
	 (T ,x)))


(defun give-ordered (list compare-func lower-bound)
  "Returnes the smallest element in LIST that is greater than LOWER-BOUND
   The order is defined by the function COMPARE-FUNC: (COMPARE-FUNC A B) should return greater than 0 if A is greater than B, 0 if A equals B or smaller than 0 if A is smaller than B"
  (cond ((not list) list)
         ((atom list) list)
	 (T (let ((current (car list))
		  (smallest-of-rest (give-ordered (cdr list) compare-func lower-bound)))
	      (cond ((not lower-bound)
		     (cond ((not smallest-of-rest) current)
			   (T (give-min current smallest-of-rest compare-func))))
		    ((not smallest-of-rest)
		     (return-if-greater current lower-bound compare-func))
		    (T
		     (let ((min (give-min current smallest-of-rest compare-func)))
		       (cond ((return-if-greater min lower-bound compare-func) min)
			     (T (return-if-greater (give-max current smallest-of-rest compare-func) lower-bound compare-func))))))))))


(defun make-ordered-stream (list comparator)
  "Gives a function that will return one element of list each time it is called ordered acc. to comparator. If all values have been returned, it will continue from the beginning" 
  (let ((lower-bound nil))
    (lambda ()
      (setf lower-bound
	    (give-ordered list comparator lower-bound)))))


(defun make-number-sequence (max start stepper)
  (let ((curr start))
    (lambda ()
      (setf curr
	    (cond ((= curr 0) max)
		  (T (funcall stepper curr)))))))

 