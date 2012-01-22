;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some numeric code, mostly solving equations
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

(defpackage :numeric
  (:use :common-lisp)
  (:export :iterate-to-fixpoint :find-root-henon
	   :find-zero-newton))

(in-package :numeric)

(defun iterate-to-fixpoint (fun x0 &optional (iter-limit 20) (diff-limit 0.001))
  "Tries to find an X satisfying X = FUN(X) by calculating the sequence X_N+1 = FUN(X_N) until ABS(X_N - X_N+1) < DIFF-LIMIT or N >= ITER_LIMIT. Starts with X_1 = FUN(X0)"
  (progn 
    (cond ((< iter-limit 1) x0)
	  (t (let ((next-x (funcall fun x0)))
	       (cond ((< (abs (- x0 next-x)) diff-limit) next-x)
		     (t (iterate-to-fixpoint fun next-x (- iter-limit 1) diff-limit))))))))


(defun find-root-henon (x x0 &optional (iter-limit 20) (diff-limit 0.001))
  "Tries to find the square root of X by using Henon's algorithm. X0 is the initial value for the iteration needed and will influence which root will be found."
	   (iterate-to-fixpoint (lambda (y) (* 0.5 (+ y (/ x y)))) x0 iter-limit diff-limit))

(defun approximate-derivation (fun &optional (dx 0.001))
  "Gives a first order approximation of the deviation of FUN"
  (lambda (x) (/ (- (funcall fun x) (funcall fun (- x dx))) dx)))


(defun find-zero-newton (fun x0 &key (iter-limit 20) (diff-limit 0.001) (derivation))
  "Tries to solve the equation 0 = FUN(X) by using Newton's algorithm. Uses X0 as initial value for the iteration. X0 will influence which solution will be found."
  (let ((deriv (cond ((not derivation) (approximate-derivation fun))
		    (t derivation))))
    (iterate-to-fixpoint (lambda (x) (- x (/ (funcall fun x) (funcall deriv x)))) x0 iter-limit diff-limit)))

