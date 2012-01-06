;; Defines a generic stream that returnes a set of elements in a certain order
;;


;; (defun compare-number (x y)
;;   (progn
;;     (print `("x   " ,x) t)
;;     (print `("y   " ,y) t)
;;    (< x y)))


;; Expects a list of non-nil elements comparable by comparator
;; (defun get-smallest (list comparator lower-limit)
;;   (progn
;;     (print list t)
;;     (cond
;;       ((not list) nil)
;;       ((not (listp list)) list)
;;       (T (let* ((current (car list))
;; 		(smallest-rest (cdr list))
;; 		(smallest-of-2 (cond ((funcall comparator lower-limit current) current)
;; 				     (T lower-limit))))
;; 	   (cond ((not smallest-rest) smallest-of-2)
;; 		 (T (progn
;; 		      (print `("smallest-rest    " ,smallest-rest))
;; 		      (let ((smallest-of-rest (get-smallest smallest-rest comparator lower-limit)))
;; 			(cond ((funcall comparator smallest-of-rest smallest-of-2)
;; 			   smallest-of-rest)
;; 			  (T smallest-of-2)))))))))))




(defun diff-1d (x  y) (- x y))

(defun diff (x y d) (- (nth d x) (nth d y)))
 
(defmacro give-min (x y comparator)
  `(cond ((< (funcall ,comparator ,x ,y) 0) ,x)
	  (T ,y)))

(defmacro give-max (x y comparator)
  `(cond ((< (funcall ,comparator ,x ,y) 0) ,y)
	 (T ,x)))

(defmacro return-if-greater (x y comparator)
  `(cond ((<= (funcall ,comparator ,x ,y) 0) nil)
	 (T ,x)))

(defun give-smallest (list comparator lower-bound)
  "Returnes the smallest element in 'list' that is greater than 'lower-bound'
   The order is defined by the function 'comparator': '(comparator a b)' should return greater than 0 if a is greater than b, 0 if a equals b or smaller than 0 if a is smaller than b"
  (cond ((not list) list)
         ((atom list) list)
	 (T (let ((current (car list))
		  (smallest-of-rest (give-smallest (cdr list) comparator lower-bound)))
	      (cond ((not lower-bound)
		     (cond ((not smallest-of-rest) current)
			   (T (give-min current smallest-of-rest comparator))))
		    ((not smallest-of-rest)
		     (return-if-greater current lower-bound comparator))
		    (T
		     (let ((min (give-min current smallest-of-rest comparator)))
		       (cond ((return-if-greater min lower-bound comparator) min)
			     (T (return-if-greater (give-max current smallest-of-rest comparator) lower-bound comparator))))))))))


(defun make-ordered-stream (list comparator)
  (let ((lower-bound nil))
    (lambda ()
      (setf lower-bound
	    (give-smallest list comparator lower-bound)))))


(defvar ordered (make-ordered-stream '((4 1 3) (2 3 15) (17 9 23) (21 0 42)) (lambda (x y) (diff x y 2))))

(funcall ordered)
			




