;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My Glorious Clustering
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


;; This code is not secured against feeding it several equal vectors.
;; If you do so, it will spit shit and then die.
;;
;; TODO
;;
;; * Evaluate only the first N components of each vector
;; * Eliminate Singularities, i.e. equal vectors
;;   -If two vectors are equal, append the last component of one vector
;;    to the other one


;;==================
;; Global parameters
;===================

;; (Yes, its so dirty un-functional, but everything else but 
;; be a painful way to go)

(defvar *min-bounding-box-len* 0.0)
(defvar *cluster-debug* nil)

;; Default file to evaluate
(defvar *file-name* nil) ;"bmovie.pp")


;; Helpers

(defmacro co-debug-out (x)
  (cond (*cluster-debug*

	 (let ((y (gensym)))
	   `(let ((,y ,x))
	      (progn
		(print ,y)
		,y))))
	(t x)))


(defmacro debug-print-execute (x &rest body)
  (cond (*cluster-debug*
	 `(progn
	    (print ,x)
	    ,@body))
	(t `(progn ,@body))))

(defun remove-all (objs list &key test)
  "Remove all elements in list OBJS from list LIST and return new list. Use function TEST to test for equality."
  (cond ((not objs) list)
	(t (cond (test (remove-all (cdr objs) (remove (car objs) list :test test)))
		 (t (remove-all (cdr objs) (remove (car objs) list)))))))



(defun split-list (vec n)
  "Splits the list VEC in two lists at position N."
  (cond ((not vec) nil)
	((< n 0) (cons nil vec))
	(t (let ((split (split-list (cdr vec) (1- n))))
	     (cons (cons (car vec) (car split)) (cdr split))))))


(defun factorial (n) 
	   (cond ((plusp n) (* n (factorial (1- n))))
		 (t 1)))


;;=====================
;; Comparator functions
;;=====================


(defun compare-mv-nd (n)
  "Returns a compare-func that uses the n-th element of two vectors to compare the vecto
rs"
  (lambda (x y) (- (nth n x) (nth n y))))


(defmacro revert-compare-func (f)
  "Returnes a compare-func that defines an order opposite to the order given by the compare-func F"
  `(lambda (x y)
     (- 0 (funcall ,f x y))))


(defun make-compare-func-seq (make-cmp-func n)
  (cond ((< n 0) nil)
	(t (append (make-compare-func-seq make-cmp-func (1- n))  (list (funcall make-cmp-func n))))))


(defun make-compare-by-dist-to-point (point)
  (lambda (x y)
    (- (squared-dist x point) (squared-dist y point))))



;;==============
;; Sorting stuff
;;==============


(Defun index-of-max (vec &optional (max nil) (index 0))
  "Returnes the index of the greatest element within list VEC."
  (cond ((not vec) index)
	(t (let ((old-max (cond ((not max) (car vec))
				(t max)))
		 (next (car vec)))
	  (cond ((<= next old-max) (index-of-max (cdr vec) old-max index))
		(t (index-of-max (cdr vec) next (1+ index))))))))


(defun give-greatest (vec cmp &optional upper-limit)
  "Returnes the greatest element in LIST that is smaller than UPPER-LIMIT
   The order is defined by the function CMP: (CMP A B) should return
   greater than 0 if A is greater than B, 0 if A equals B or smaller than
   0 if A is smaller than B"
  (progn
    (cond ((not vec) nil)
	      (T
	       (let ((curr (cond
			     (upper-limit (cond
				      ((< (funcall cmp (car vec) upper-limit) 0)
					       (car vec))
					      (T nil)))
				 (T (car vec))))
		     (greatest-rest (give-greatest (cdr vec) cmp upper-limit)))
		 (cond ((not curr) greatest-rest)
		       ((not greatest-rest) (cons curr nil))
		       (T (let ((cmp-result (funcall cmp curr (car greatest-rest))))
			    (cond ((< cmp-result 0) greatest-rest)
				  ((> cmp-result 0) (cons curr nil))
				  (T (cons curr greatest-rest)))))))))))
		

(defun make-ordered-stream (vec cmp)
  "Gives a function that, when called several times, will return each element in vec in order. After the last element has been returned, the function will return NIL, and then start over again."
  (let ((last nil))
    (lambda ()
      (let ((next (give-greatest vec cmp last)))
	(cond ((not next) (setf last nil))
	      (t (progn
		   (setf last (car next))
		   next)))))))


(defun make-ordered-list (vec cmp &optional give-next)
  "Bubble-Sort. Builds a vector by sequentially calling GIVE-NEXT until NIL is returned and collects the return values."
  (let* ((next (cond (give-next give-next)
		    (t (make-ordered-stream vec cmp))))
	(next-el (funcall next)))
    (cond ((not next-el) nil)
	  (t (cons next-el (make-ordered-list vec cmp next))))))



;;========================
;; Simple vector functions
;;========================


(defun vec-norm (x)
  "Calculates the eucledian norm of a vector. The vector consists of a lists of numbers"
  (sqrt (reduce #'+ (mapcar #'* x))))


(defun vector-add (x)
  "Takes a list of vectors that represent vectors. Returnes the vector sum of all of these vectors"
	   (cond ((or (not x) (atom x) (atom (car x))) x)
		 (t (labels ((vector-add-2 (x y)
			       (cond ((not x) y)
				     ((not y) x)
				     (t (cons (+ (car x) (car y)) (vector-add-2 (cdr x) (cdr y)))))))
		    (vector-add-2 (car x) (vector-add (cdr x)))))))


(defun vector-scale (x vecs)
  "Takes a list of vectors and multiplies them by the scalar x"
  (mapcar (lambda (vec) (mapcar (lambda (el) (* x el)) vec)) vecs))


(defun squared-dist (x y)
  "Returnes the squared distance in between the vectors X and Y or 0 if either of them is not given"
  (cond ((or (not x) (not y)) 0)
	(t (let ((diff (- (car x) (car y))))
	     (+ (* diff diff) (squared-dist (cdr x) (cdr y)))))))


(defun vector-comp (x y)
  "Compares two vectors X and Y by assuming numeric elements and comparing element by element."
  (cond ((not x) (cond ((not y) t)
			(t nil)))
	 ((not y) nil)
	 ((atom x) (eq x y))
	 (t (cond ((eq (car x) (car y)) (vector-comp (cdr x) (cdr y)))
		  (t nil)))))


;;==============================================================================
;; Functions considering a list of point clouds, where each point cloud consists
;; of a list of points/vectors. I.e. these functions operate with lists of lists
;; (= point cloud) containing lists (=points/vectors)
;;==============================================================================


(defun bounding-box-len  (vec cmp)
  "Returnes an array with the side lens of the bounding box for the vectors given in VEC. Uses norms CMP. If all points lie within a plain, the bounding box would collapse with 0 volume. To avoid this, *MIN-BOUNDING-BOX-LEN* is used as minimum length"
  (cond ((not cmp) nil)
	(t (let* ((cmp-1 (car cmp))
		  (max (car (give-greatest vec cmp-1)))
		  (min (car (give-greatest vec (revert-compare-func cmp-1)))))
	     (cons
	      (let ((len (funcall cmp-1 max min)))
		(cond ((< len *min-bounding-box-len*) *min-bounding-box-len*)
		      (t len)))
	      (bounding-box-len vec (cdr cmp)))))))


(defun vec-of-vec-len (vec)
  "Takes a list of lists and returnes the number of elements contained within each of these lists."
  (cond ((not vec) 0)
	(t (+  (list-length (car vec)) (vec-of-vec-len (cdr vec))))))


(defun vec-of-vec-density (vec cmp)
  "Calculates the density of VEC. The volume of VEC is calculated by the CMP functions"
  (let ((vol (reduce #'* (bounding-box-len vec cmp))))
    (cond ((= 0 vol) 'singularity)
	  (t (/ (vec-of-vec-len vec) vol)))))


(defun density-bounding-rectangle (vec cmp)
  "Calculates the density of VEC. The volume of VEC is calculated by the CMP functions"
  (let ((vol (co-debug-out (reduce #'* (co-debug-out (bounding-box-len vec cmp))))))
    (cond ((= 0 vol) 'singularity)
	  (t (/ (list-length vec) vol)))))


(defmacro compare-neighbour (vec)
	 `(lambda (x y)
	    (- (squared-dist y ,vec) (squared-dist x ,vec))))


(defun get-pom (vec-list)
  "Takes a list of vectors and returnes the point of mass of these vectors"
  (labels ((vec-div (x y)
	     "Divide each element of X by Y"
	     (cond ((not x) nil)
		   ((atom x) (/ x y))
		   (t (cons (/ (car x) y) (vec-div (cdr x) y))))))
    (vec-div (vector-add vec-list) (list-length vec-list))))


(defun vec-of-vec-density-balls (vec cmp &optional (dim nil))
  "Returnes the density of the cloud VEC. The density is calculated referring to the volume of a cube with a length of its sides from the point of mass to the most distant point"
  (cond ((not vec) 0)
	((atom vec) 0)
	(t
	 (let* ((pom (get-pom vec))
		(dist-func (make-compare-by-dist-to-point pom))
		(radius (squared-dist (car (give-greatest vec dist-func)) pom))
		(n (cond (dim dim)
			 (t (length (car vec))))))
	   (expt radius n))))) 


;;================================
;; Particular clustering functions
;;================================


(defmacro density (&rest args)
   " Density function to use for clustering"
  `(vec-of-vec-density-balls ,@args))


(defun give-min-cluster (point vec)
  (let* ((next (make-ordered-stream vec (compare-neighbour point)))
	 (next-point (progn (funcall next) (funcall next))))
    (cond (next-point (cons point next-point))
	  (t nil))))


(defun cluster (point-cloud vec cmp-list lower-density &optional (method :global-density))
  "Find cluster around POINT-CLOUD from points in VEC.
   CMP-LIST is a list of COMPARE-FUNCTIONS that define the metric on VEC.
   LOWER-DENSITY gives the current density - the exact meaning depends on METHOD.
   METHOD determines the way to cluster. It can take several SYMBOLS as values:
      :GLOBAL-DENSITY   Stop clustering if the density of the cluster found underruns LOWER-DENSITY.
      :MAX-DENSITY      Stop clustering as soon as the density starts decreasing."
  (let* ((pom (co-debug-out (get-pom point-cloud)))
	 (next-point
	  (co-debug-out (funcall (make-ordered-stream vec (compare-neighbour pom))))))
    (cond ((not next-point) (list lower-density point-cloud)) ; There is no more point to add to cluster
	  ( t
	   (let* ((new-cloud (co-debug-out (union point-cloud next-point)))
		  (new-density (co-debug-out (density new-cloud cmp-list))))
	     (cond ((<= new-density lower-density)
		    (debug-print-execute
		     (list lower-density " is bigger than " new-density)
		     (list lower-density point-cloud)))
		   (t (debug-print-execute
		       (list new-cloud lower-density " is smaller than " new-density)
		       (cluster new-cloud (remove-all next-point vec) cmp-list
				(cond ((eq method :global-density) lower-density)
				      ((eq method :max-density) new-density)))))))))))


(defun calc-distances (point-cloud vec n last-dist cmp)
  "Starts with VEC, calculates the next closest point to VEC in POINT-CLOUD, then
   creates a list of VEC and that next closest point. Then, calculates the point of mass for that list, searches the next closest point to the point of mass in POINT-CLOUD, and so on. Returnes a list of pairs, where each pair contains the point as well as the change of the distance"
  (cond ((null point-cloud) nil)
	(t
	 (let* ((proximus (give-greatest point-cloud (compare-neighbour vec)))
		(new-pom (vector-scale (/ 1.0 (1+ n)) (list (vector-add (append (vector-scale n (list vec)) proximus)))))
		(new-dist (funcall cmp (car proximus) (car new-pom)))
		(new-pair (cons (- new-dist last-dist) proximus)))
	   (debug-print-execute
	    (list "proximus" proximus "new-pom" new-pom "new-dist" new-dist)
	    (cons new-pair
		  (calc-distances (remove (car proximus) point-cloud) (car new-pom)
				  (1+ n) new-dist cmp)))))))
 

(defun list-to-extremum (weighted-cloud selector)
  "Gives list consisting of all points of WEIGHTED-CLOUD up to a certain point.
   WEIGHTED-CLOUD is a list of pairs of a list and its weight. SELECTOR is a function evaluating the WEIGHT if the corresponding entry should be used. The first WEIGHT being evaluated to NIL by SELECTOR ends the list to be returned."
  (cond ((null weighted-cloud) nil)
	((funcall selector (car (car weighted-cloud))) (cons (car (cdr (car weighted-cloud))) (list-to-extremum (cdr weighted-cloud) selector)))
	(t nil)))


;;;-------------------------------------------
;;;   Cluster resp. distance to Center of Mass
;;;-------------------------------------------


(defun cluster-max-distance (point-cloud vec &optional (arbiter #'length))
  "Find cluster around point VEC within POINT-CLOUD"
  (let* ((weighted (calc-distances point-cloud vec 1 0.0 #'squared-dist))
	 (max-dist (car (car (give-greatest weighted (lambda (x y) (- (car x) (car y)))))))
	 (cluster (list-to-extremum weighted (lambda (x) (< x max-dist)))))
    (list (funcall arbiter cluster) (append (list vec) cluster))))


(defun make-cluster-distance-stream (point-cloud)
  "Make stream of clusters ordered by the number of their points"
  (labels ((make-clusters (cloud)
	     (cond ((null cloud) nil)
		   (t (let ((next (car cloud)))
			(progn
			  (format *error-output* "Clustering round point ~a~%" next)
			  (cons (cluster-max-distance point-cloud next)
				(make-clusters (cdr cloud)))))))))
    (let ((cluster-list (make-clusters point-cloud))
	  (last nil))
	(lambda ()
	  (progn
	  (setf last  (give-greatest cluster-list
				     (lambda (x y)
				       (- (car x) (car y)))
				     (car last)))
	  (cond (last
		 ;; (progn
		 ;;   (print last)
		 ;; (cdr (car last))))
		 last)
		(t nil)))))))


;;;------------------------
;;;   Cluster resp. density
;;;------------------------
	   

(defun cluster-round-point (x vec n &optional (method :global-density))
  "Find cluster round point X in point set VEC. The points have N dimensions.
   As a kickstart for CLUSTER, this function takes the optional argument METHOD that is 
   just passed through to CLUSTER. 
   Uses standard metrics."
  (let ((min-cluster (give-min-cluster x vec)))
    (cluster min-cluster (remove-all min-cluster vec :test #'vector-comp) 
	     (make-compare-func-seq #'compare-mv-nd n)
	     (density vec (make-compare-func-seq #'compare-mv-nd n)) method)))


(defun cluster-all (vec n &optional (method :global-density))
  "Creates clusters of points within the point cloud VEC. This method needs the points' dimension N."
  (mapcar (lambda (x)
	    (cluster-round-point x vec n method)) vec))


(defun cluster-with-max-density (vec n &optional (method :global-density))
  "Returnes the cluster of points from point cloud VEC with the highest density.
   This function needs the points's dimension N."
  (labels ((compare-cluster (x y)
	     (- (car x) (car y))))
  (give-greatest (cluster-all vec n method) #'compare-cluster)))



;;=============
;; Main program
;;=============

(load "io.lisp")

(defun main-density ()
  "Main program to be called when compiled as stand-alone"
  (print (cluster-with-max-density (io:read-vectors-from-list *file-name*) 8)))


(defun print-all-clusters (get-next)
  (labels ((print-next (next)
	     (cond ((not next) nil)
		   (t
		      (mapcar
		       (lambda (x)
			 (mapcar
			  (lambda (y)
			    (progn
			      (format t "# ~$ ~%" (length y))
			      (io:output-table y))) (cdr x))) next)
		      (print-next (funcall get-next))))))
    (print-next (funcall get-next))))


(defun main ()
  (print-all-clusters
   (make-cluster-distance-stream
    (io:read-vectors-from-list *file-name*))))


;;===========
;; Test Stuff
;;===========


;; (defvar test-data '((1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8) (30 30)))

;; ;; (setf test-data '((120 10) (1000 99) (1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8) (30 30) (10001 254)))


;; (defvar test-ordered-stream (make-ordered-stream test-data (compare-mv-nd 1)))

;; (defvar test-ordered-list (make-ordered-list test-data (compare-mv-nd 1)))

;; (defvar cmp-funcs (make-compare-func-seq #'compare-mv-nd 1))


;; ;; I/O
;; (let ((instream (open "test.csv" :direction :input)))
;; 	   (print (read-vector instream))
;; 	   (close instream))