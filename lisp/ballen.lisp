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


(defun bubble-sort (list cmp &optional lower)
  "Sorts list LIST using bubble sort. It uses CMP for comparing two elements."
  (cond ((not list) nil)
	(T
	 (let ((next (give-ordered list cmp lower)))
	   (cond ((not next) nil)
		 (T (cons next (bubble-sort list cmp next))))))))



(defvar test-data '((1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8)))


(defun calc-densities (ordered-list cmp &optional  (n 0) (len 0))
  "calculates a list of densities. ORDERED-LIST is expected to be a list of lists. Of each list within ORDERED-LIST, the first entry is used for calculating the densities."
  (cond ((not ordered-list) NIL)
	(T (let ((a (car ordered-list))
		 (b (car (cdr ordered-list))))
	     (cond ((or (not a) (not b)) nil)
		   (t (let* ((dist (abs (funcall cmp a b)))
			     (len-invert (cond ((and (= 0 len) (= 0 dist)) 1)
					       (t (/ 1 (+ len (/ dist 2)))))))
			     (cons (* len-invert (1+ n))
			       (calc-densities (cdr ordered-list) cmp (/ 1 len-invert))))))))))
				    ;; 	       (
				    ;; 	      (1+ n)
				    ;; (cons (/ (1+ n) (+ len (/ dist 2)))
				    ;; 	  (calc-densities (cdr ordered-list) cmp (1+ n) (+ len dist))))))))))


(Defun index-of-max (vec &optional (max nil) (index 0))
  "Returnes the index of the greatest element within list VEC."
  (cond ((not vec) index)
	(t (let ((old-max (cond ((not max) (car vec))
				(t max)))
		 (next (car vec)))
	  (cond ((<= next old-max) (index-of-max (cdr vec) old-max index))
		(t (index-of-max (cdr vec) next (1+ index))))))))



(defun split-list (vec n)
  "Splits the list VEC in two lists at position N."
  (cond ((not vec) nil)
	((< n 0) (cons nil vec))
	(t (let ((split (split-list (cdr vec) (1- n))))
	     (cons (cons (car vec) (car split)) (cdr split))))))


(defun cluster-vec (vec cmp)
  (let ((ordered-list (bubble-sort vec cmp nil)))
    (split-list ordered-list (index-of-max (calc-densities vec cmp)))))


(defun give-greatest (vec cmp &optional upper-limit)
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
  (let ((last nil))
    (lambda ()
      (let ((next (give-greatest vec cmp last)))
	(cond ((not next) (setf last nil))
	      (t (progn
		   (print last)
		   (setf last (car next))
		   next)))))))


(defun make-ordered-list (vec cmp &optional give-next)
  (let* ((next (cond (give-next give-next)
		    (t (make-ordered-stream vec cmp))))
	(next-el (funcall next)))
    (cond ((not next-el) nil)
	  (t (cons next-el (make-ordered-list vec cmp next))))))

(defun calc-densities-with-equals (ordered-list cmp &optional  (n 0) (len 0))
  "calculates a list of densities. ORDERED-LIST is expected to be a list of lists. Of each list within ORDERED-LIST, the first entry is used for calculating the densities."
  (progn
    (print ordered-list)
    (print n)
    (print len)
    (print "-----------------------")
  (cond ((not ordered-list) NIL)
	(T (let ((list-a (car ordered-list))
		 (list-b (car (cdr ordered-list))))
	     (progn
	       (print list-a)
	       (print list-b)
	       (print (car list-a))
	       (print (car list-b))
	     (cond ((not (and list-a  list-b (car list-a) (car list-b))) nil)
		   (t (let* ((a (car list-a))
			     (b (car list-b))
			     (n-new (cond ((not n) (length list-a))
					  (t n)))
			     (dist (abs (funcall cmp a b)))
			     (len-invert (cond ((= 0 len) 0)
					     (t (/ 1 len))))
			     (len-new-invert (cond ((= 0 dist) len-invert)
						   (t (/ 1 (+ len (/ dist 2)))))))
			(progn
			(print len-invert)
			(cons (- (* len-new-invert (+ n-new (length list-b))) (* n-new len-invert))
			       (calc-densities-with-equals (cdr ordered-list) cmp (+ n-new (length list-b)) (cond ((= 0 len-new-invert) 0) (t (/ 1 len-new-invert)))))))))))))))


(defvar test-ordered-stream (make-ordered-stream test-data (compare-mv-nd 1)))

(defvar test-ordered-list (make-ordered-list test-data (compare-mv-nd 1)))

