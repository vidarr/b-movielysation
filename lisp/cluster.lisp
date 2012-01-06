;;; -*- Mode: Lisp; Syntax: Lisp -*-
;;; My Glorious Clustering
;;; (C) 2011 Michael J. Beer

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
(defvar *file-name* "bmovie.pp")


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


;;==============
;; I/O Functions				
;;==============

;; These functions are way less efficient than the inefficient stuff above
;; Mainly for discovering Common Lisp
;; Never to be used !


(defun get-token (&key stream type)
  "Returnes a token read from stream (STDIN if not given) and tries to convert it to TYPE (String if not given)"
  (labels ((get-token-as-list (read-something)
	     (let ((c (read-char stream nil)))
	       (cond ((or (not c) (eq c 'eof-value)) nil)
		     ((eq c #\ )
		      (cond (read-something nil)
			    (t (get-token-as-list t))))
		     ((eq c #\Newline)
		      (cond (read-something (unread-char c stream))
			    (t c)))
		     (t (let ((rest (get-token-as-list t)))
			  (cons c (cond ((eq rest #\Newline) nil)
					(t rest)))))))))
    (let ((token (get-token-as-list nil)))
      (cond ((not token) nil)
	    ((eq token #\Newline) nil)
	    (t (coerce token (cond ((eq type nil) 'string)
					     (t type))))))))


(defun read-vector (&optional in-stream (getter #'get-token))
  "Reads in several tokens from STREAM (STDIN if none given) and returns them as CONS list. Uses GETTER to retrieve the tokens (GET-TOKEN if none given)."
  (let ((token (funcall getter :stream in-stream)))
    (cond ((not token) nil)
	  (t (cons token (read-vector in-stream getter))))))


(defun read-vectors-from-list (name)
  "Read in subsequent vectors from a file NAME untit an empty line is discovered."
  (labels ((read-vector-list (stream)
	   (let ((next (mapcar #'read-from-string (read-vector stream))))
	     (cond ((not next) nil)
		   (t (cons next (read-vector-list stream)))))))
    (let* ((instream (open name :direction :input))
	   (*read-eval* nil)
	   (vectors (read-vector-list instream)))
      (close instream)
      vectors)))


(defun lispify (vec)
  "Takes a list of strings, have all of them evaluated by clisp and return new list with the results"
  (let ((*read-eval* nil))
    (mapcar #'read-from-string vec)))



(defun output-table (table &optional (dest t))
  "Prints a table (a list containing lists of numbers) to stream DEST one row at a line"
  (labels ((output-line (line)
	     (cond ((not line) (format dest "~%"))
		   ((atom line) (format dest "~G~%" line))
		   (t (progn
			(format dest "~G" (car line))
			(output-line (cdr line)))))))
    (cond ((not table) nil)
	  (t (progn
	       (output-line (car table))
	       (output-table (cdr table)))))))



;;=============
;; Main program
;;=============


(defun main ()
  "Main program to be called when compiled as stand-alone"
  (print (cluster-with-max-density (read-vectors-from-list *file-name*) 8)))


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