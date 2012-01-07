;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My Glorious Clustering - Scheme version
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

;; This is not up to date - it does not have neat input/output facilities 
;; and supports clsutering by density only.

;;=======================================================
;; Functions implemented in Common Lisp but not in Scheme
;;=======================================================


(define nth (lambda (n list)
	      (cond ((null? list) #f)
		    ((not (pair? list))
		     (cond ((= 0 n) list)
			   (else '())))
		    ((= 0 n) (car list))
		    (else
		     (nth (1- n) (cdr list))))))


(define reduce (lambda (func list)
		 (cond ((not (pair? list)) '())
		       ((null? (cdr list)) (car list))
		       (else
			(func (car list) (reduce func (cdr list)))))))


(define remove (lambda (el vec test)
		 (cond ((null? vec) '())
		       (else
			(cond ((not (test el (car vec)))
			       (cons (car vec) (remove el (cdr vec) test)))
			      (else
			       (remove el (cdr vec) test)))))))


(define member? (lambda (x y test)
		  (cond ((null? y) #f)
			((not (pair? y)) #f)
			((test x (car y)) #t)
			(else (member? x (cdr y) test)))))


(define union (lambda (x y test)
		(begin
		  (cond ((null? x) y)
			((null? y) x)
			((member? (car y) x test) (union x (cdr y) test))
			(else (cons (car y) (union x (cdr y) test)))))))


;;=======================================
;; Programm as in CL, converted to Scheme
;;=======================================


(define print-list (lambda (vec)
		     "Prints a list to STDOUT"
		     (cond ((null? vec) #t)
			   ((not (pair? vec)) (display vec))
			   (else
			    (begin
			      (print-list (car vec))
			      (display "   ")
			      (print-list (cdr vec)))))))


(define remove-all (lambda (objs list test)
		     "Remove all elements in list OBJS from list LIST and return new list. Use function TEST to test for equality."
		     (begin
		       (cond ((null? objs) list)
			     (else (remove-all (cdr objs) (remove (car objs) list test) test))))))


(define split-list (lambda (vec n)
		     "Splits the list VEC in two lists at position N."
		     (cond ((null? vec) '())
			   ((< n 0) (cons '() vec))
			   (else (let ((split (split-list (cdr vec) (1- n))))
				   (cons (cons (car vec) (car split)) (cdr split)))))))


;;=====================
;; Comparator functions
;;=====================


(define compare-mv-nd (lambda (n)
			"Returns a compare-func that uses the n-th element of two vectors to compare the vecto
rs"
			(lambda (x y) (- (nth n x) (nth n y)))))


(define revert-compare-func (lambda (f)
			      "Returnes a compare-func that defines an order opposite
                               to the order given by the compare-func F"
			      (lambda (x y)
				(- 0 (f x y)))))


(define make-compare-func-seq (lambda (make-cmp-func n)
				"Creates a list of (MAKE-CMP-FUNC I) functions for I equals 0 through N"
				(cond ((< n 0) '())
				      (else (append (make-compare-func-seq make-cmp-func (1- n))
						    (list (make-cmp-func n)))))))


;;==============
;; Sorting stuff
;;==============


(define index-of-max (lambda (vec max index)
		       "Returnes the index of the greatest element within list VEC."
		       (cond ((null? vec) index)
			     (else (let ((old-max (cond ((null? max) (car vec))
							(t max)))
					 (next (car vec)))
				     (cond ((<= next old-max) (index-of-max (cdr vec) old-max index))
					   (else (index-of-max (cdr vec) next (1+ index)))))))))



(define give-greatest (lambda  (vec cmp upper-limit)
			"Returnes the greatest element in LIST that is smaller than UPPER-LIMIT
   The order is defined by the function CMP: (CMP A B) should return
   greater than 0 if A is greater than B, 0 if A equals B or smaller than
   0 if A is smaller than B"
			(cond ((null? vec) '())
			      (else
			       (let ((curr (cond
					    ((not (null? upper-limit)) (cond
									((< (cmp (car vec) upper-limit) 0)
									 (car vec))
									(else '())))
					    (else (car vec))))
				     (greatest-rest (give-greatest (cdr vec) cmp upper-limit)))
				 (cond ((null? curr) greatest-rest)
				       ((null? greatest-rest) (cons curr '()))
				       (else (let ((cmp-result (cmp curr (car greatest-rest))))
					       (cond ((< cmp-result 0) greatest-rest)
						     ((> cmp-result 0) (cons curr '()))
						     (else (cons curr greatest-rest)))))))))))


(define make-ordered-stream (lambda (vec cmp)
			      "Gives a function that, when called several times, will return each element in vec in order. After the last element has been returned, the function will return NIL, and then start over again."
			      (let ((last '()))
				(lambda ()
				  (let ((next (give-greatest vec cmp last)))
				    (begin
				      (cond ((null? next) (set! last '()))
					    (else 
					     (set! last (car next))))
				      next))))))


(define  make-ordered-list (lambda  (vec cmp give-next)
			     "Bubble-Sort. Builds a vector by sequentially calling GIVE-NEXT until NIL is returned and collects the return values."
			     (let* ((next (cond (give-next give-next)
						(else (make-ordered-stream vec cmp))))
				    (next-el (next)))
			       (cond ((null? next-el) '())
				     (else (cons next-el (make-ordered-list vec cmp next)))))))


;;========================
;; Simple vector functions
;;========================


(define vec-norm (lambda (x)
		   "Calculates the eucledian norm of a vector. The vector consists of a lists of numbers"
		   (let ((sqr (lambda (x) (* x x))))
		     (sqrt (reduce + (map sqr x))))))


(define vector-add (lambda (x)
		     "Takes a list of vectors that represent vectors. Returnes the vector sum of all of these vectors"
		     (cond ((or (null? x) (not (pair? x)) (not (pair? (car x)))) x)
			   (else (letrec (
					  (vector-add-2
					   (lambda (x y)
					     (cond ((null? x) y)
						   ((null? y) x)
						   (else (cons (+ (car x) (car y)) (vector-add-2 (cdr x) (cdr y))))))))
				   (vector-add-2 (car x) (vector-add (cdr x))))))))


(define vector-scale (lambda (x vecs)
		       "Takes a list of vectors and multiplies them by the scalar x"
		       (map (lambda (vec) (map (lambda (el) (* x el)) vec)) vecs)))


(define squared-dist (lambda (x y)
		       (cond ((or (null? x) (null? y)) 0)
			     (else (let ((diff (- (car x) (car y))))
				     (+ (* diff diff) (squared-dist (cdr x) (cdr y))))))))


(define vector-comp (lambda (x y)
		      "Compares two vectors X and Y by assuming numeric elements and comparing element by element."
		      (cond ((null? x) (cond ((null? y) #t)
					     (else #f)))
			    ((null? y) #f)
			    ((not (pair? x)) (eq? x y))
			    (else (cond ((eq? (car x) (car y)) (vector-comp (cdr x) (cdr y)))
					(else #f))))))


;;==============================================================================
;; Functions considering a list of point clouds, where each point cloud consists
;; of a list of points/vectors. I.e. these functions operate with lists of lists
;; (= point cloud) containing lists (=points/vectors)
;;==============================================================================


(define bounding-box-len (lambda (vec cmp)
			   "Returns the dimensions of the bounding box of VEC. Expects CMP as a list of comparator functions that determine the dimensions"
			   (cond ((null? cmp) '())
				 (else (let* ((cmp-1 (car cmp))
					      (max (car (give-greatest vec cmp-1 '())))
					      (min (car (give-greatest vec (revert-compare-func cmp-1) '()))))
					 (cons (cmp-1 max min) (bounding-box-len vec (cdr cmp))))))))


(define vec-of-vec-len (lambda (vec)
			 "Takes a list of lists and returnes the number of elements contained within each of these lists."
			 (cond ((null? vec) 0)
			       (else (+  (length (car vec)) (vec-of-vec-len (cdr vec)))))))


(define vec-of-vec-density (lambda (vec cmp)
			     "Calculates the density of VEC. The volume of VEC is calculated by the CMP functions"
			     (let ((vol (reduce * (bounding-box-len vec cmp))))
			       (cond ((= 0 vol) 'singularity)
				     (t (/ (vec-of-vec-len vec) vol))))))


(define density (lambda (vec cmp)
		  "Calculates the density of VEC. The volume of VEC is calculated by the CMP functions"
		  (let ((vol (reduce * (bounding-box-len vec cmp))))
		    (cond ((= 0 vol) 'singularity)
			  (else (/ (length vec) vol))))))


(define compare-neighbour (lambda (vec)
			    (lambda (x y)
			      (- (squared-dist y vec) (squared-dist x vec)))))


(define get-pom (lambda (vec-list)
		  "Takes a list of vectors and returnes the point of mass of these vectors"
		  (letrec ((vec-div (lambda (x y)
				      "Divide each element of X by Y"
				      (cond ((null? x) '())
					    ((not (pair? x)) (/ x y))
					    (else (cons (/ (car x) y) (vec-div (cdr x) y)))))))
		    (vec-div (vector-add vec-list) (length vec-list)))))



;;=====================
;; Clustering functions
;;=====================


(define give-min-cluster (lambda (point vec)
			   (let* ((next (make-ordered-stream vec (compare-neighbour point)))
				  
				  (next-point (begin
						(next) (next))))
			     (cond (next-point (cons point next-point))
				   (else '())))))


(define cluster (lambda (point-cloud vec cmp-list lower-density method)
		  "Find cluster around POINT-CLOUD from points in VEC.
   CMP-LIST is a list of COMPARE-FUNCTIONS that define the metric on VEC.
   LOWER-DENSITY gives the current density - the exact meaning depends on METHOD.
   METHOD determines the way to cluster. It can take several SYMBOLS as values:
      :GLOBAL-DENSITY   Stop clustering if the density of the cluster found underruns LOWER-DENSITY.
      :MAX-DENSITY      Stop clustering as soon as the density starts decreasing."
		  (let* ((pom (get-pom point-cloud))
			 (next (make-ordered-stream vec (compare-neighbour pom)))
			 (next-point (next)))
		    (cond ((null? next-point) (list lower-density point-cloud)) 
			  (else
			   (let* ((new-cloud (union point-cloud next-point equal?))
				  (new-density (density new-cloud cmp-list)))
			     (cond ((<= new-density lower-density)
				    (list lower-density point-cloud))
				   (else
				    (cluster new-cloud (remove-all next-point vec equal?) cmp-list
					     (cond ((eq? method 'global-density) lower-density)
						   ((eq? method 'max-density) new-density))
					     method)))))))))


(define cluster-round-point (lambda (x vec n method)
			      "Find cluster round point X in point set VEC. The points have N dimensions.
   As a kickstart for CLUSTER, this function takes the optional argument METHOD that is 
   just passed through to CLUSTER. 
   Uses standard metrics."
			      (let ((min-cluster (give-min-cluster x vec)))
				(cluster min-cluster (remove-all min-cluster vec vector-comp) 
					 (make-compare-func-seq compare-mv-nd n)
					 (density vec (make-compare-func-seq compare-mv-nd n)) method))))


;;===========
;; Test stuff
;;===========

(define test-data '((1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8) (30 30)))

;; (setf test-data '((120 10) (1000 99) (1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8) (30 30) (10001 254)))


(define test-ordered-stream (make-ordered-stream test-data (compare-mv-nd 1)))

;; (define test-ordered-list (make-ordered-list test-data (compare-mv-nd 1)))

(define cmp-funcs (make-compare-func-seq compare-mv-nd 1))
