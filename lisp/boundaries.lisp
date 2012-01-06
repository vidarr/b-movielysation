(load "sequence.lisp")

(defun give-vec-min (list n)
  (mb-sequence::give-ordered list (mb-sequence::compare-mv-nd n) nil))

(defun give-vec-max (list n)
  (mb-sequence::give-ordered list (mb-sequence::revert-compare-func (mb-sequence::compare-mv-nd n)) nil))


(defvar test-data '((1 1) (1 3) (3 3) (4 2) (5 1) (4 5) (9 7) (9 10) (10 5) (12 8)))

(defun bounding-len (vec cmp)
  (abs (funcall cmp (mb-sequence::give-ordered vec cmp nil) (mb-sequence::give-ordered vec (mb-sequence::revert-compare-func cmp) nil))))


(defun bounding-box (vec cmp)
  (cond (cmp (cons (bounding-len vec (car cmp)) (bounding-box vec (cdr cmp))))
	(T nil)))


(defun make-cmp-sequence (n &optional (c 0))
  (cond ((not (<= n c))
	 (cons (mb-sequence::compare-mv-nd c) (make-cmp-sequence (1- n) (1+ c))))
	(T NIL)))


(defun vec-len (vec)
  (cond (vec (1+ (vec-len (cdr vec))))
	(T 0)))


(defun make-own-sequence (n)
  (cond ((>= n 0) (cons n (make-own-sequence (1- n))))
	(T nil)))


(defun make-area (vec &optional cmp)
  (let ((bounding-box NIL) ;(gensym))
	(compare-func cmp) ;(gensym))
	(list vec) ;(gensym))
        (current-dim 0)) ;(gensym)))
    (progn
      (setf compare-func
	    (cond (cmp cmp)
		  (T (make-cmp-sequence (vec-len vec)))))
      (lambda (sym &optional val)
	(cond ((eq sym :BOUNDING-BOX) (cond (val (setf bounding-box val))
					    (T bounding-box)))
	      ((eq sym :COMPARE-FUNC) (cond (val (setf compare-func val))
					    (T compare-func)))
	      ((eq sym :LIST) (cond (val (setf list val))
				    (T list)))
	      ((eq sym :CURRENT-DIM) (cond (val (setf current-dim val))
					   (T current-dim)))
	      (T NIL))))))
      
  
(defun calc-volume (vec)
  (reduce #'* vec))


(defun rest-of-sequence (next)
  (let ((x (funcall next)))
    (cond ((not x)  x))
	  (T (cons x (rest-of-sequence next))))))

(defun split-recursive (left x next-el dim n compare-func bounding-box diff-density)
  (let ((y (funcall next-el)))
    (progn
      (cond ((not y) left)
	    (let ((lbox (copy-list bounding-box))
		  (rbox (copy-list bounding-box))
		  (norm (nth dim compare-func))
		  (new-left (append y left))
		  (old-diff (nth dim bounding-box))
		  (half-dist (abs (/ (funcall (cmp x y)) 2))))
	      (progn
		(setf (nth dim rbox) (- old-diff half-diff))
		(setf (nth dim lbox) (+ old-diff half-diff))
		(cond ((or (= (nth dim rbox) 0) (= nth dim lbox)) nil)
		      (T (let ((len-left (vec-len new-left))
			       (new-diff-density (abs (- (/ len-left (calc-volume lbox)) (/ (- n len-left) (calc-volume rbox))))))
			   (cond ((> diff-density new-diff-density)
				  (cons left (rest-of-sequence next-el)))
				 (T (split-recursive new-left y next-el dim n compare-func bounding-box new-diff-density))))))))))))

		  
		  
	      
	      
  (let* ((y (funcall next-el))
	 (lbox (copy-list bounding-box))
         (rbox (copy-list bounding-box))
	 (cmp (nth dim compare-func))
	 (old-diff (nth dim bounding-box))
	 (half-diff (/ (abs (funcall cmp x y)) 2))
	 (new-left (append left x)))
    (progn
      (setf (nth dim rbox) (- old-diff half-diff))
      (setf (nth dim lbox) half-diff)
      (let ((new-diff-density (abs (- (/ (vec-len new-left) (calc-volume lbox))
                                   (/ (- n (vec-len new-left)) (calc-volume rbox))))))
	(cond ((<= diff-density new-diff-density)
	       (split-recursive new-left y next-el dim n
				compare-func bounding-box new-diff-density))
	      (T (cons left next-el)))))))


(defun split-set-recursive (left x  n compare-func bounding-box diff-density)
  (let* ((x (car right))
	 (left-new (cons left x))
	 (hdiff (abs (- (nth (cdr right))
	 (new-diff-density (abs (- (/ (vec-len left-new) (calc-volume left-new))
				   (/ (vec-len right) (calc-volume right))))))
    (cond ((< new-diff-density diff-density)
	   (split-recursive left-new (cdr right) n compare-func bouding-box new-diff-density))
	  (T (cons left right)))))

(defun split-set (list cmp stepper bounding-box)
  (let ((dim (funcall stepper))
	(compare-func (nth cmp))
	(get-el (mb-sequence::make-ordered-stream list compare-func))
	(left NIL) (right NIL))
    (

