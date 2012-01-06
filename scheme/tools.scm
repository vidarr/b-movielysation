;;; Create sequence starting at number s and up to number e
(define create-sequence
  (lambda (s e)
    (cond ((= s e) (list s))
          ((not (< s e)) (create-sequence e s))
          (else (cons s (create-sequence (+ 1 s) e))))))

;;; Get nth element of a list
(define get-nth
  (lambda (x n)
    (cond ((or (< n 0) (not (list? x))) #f)
          (else
           (cond ((null? x) #f)
                 ((= n 0) (car x))
                 (else (get-nth (cdr x) (- n 1))))))))

;;; Takes a list of vectors, creates a new list consisting of each vectors' componen
(define create-list-of-nth-elements
  (lambda (x n)
    (cond ((or (< n 0) (not (list? x))) #f)
          ((null? x) '())
          (else (cons (get-nth (car x) n) (create-list-of-nth-elements (cdr x) n))))))

;;; Filters a list: Returns a list of all elements that satisfy criterium
;;; Criterium is a function of one argument
(define filter-list
  (lambda (l criterium)
    (cond ((null? l) '())
          (else
           (let ((el (car l)) (rest (cdr l)))
             (cond ((criterium el) (cons el (filter-list rest criterium)))
                   (else (filter-list rest criterium))))))))

;;; Filters a list: Returns a list consisting of #t or #f for each element, dep. on whether el satisfies the criterium 
;;; Criterium is a function of one argument
(define filter-list-tf
     (lambda (l criterium)
         (cond ((null? l) '())
               (else
                (let ((el (car l)) (rest (cdr l)))
                  (cond ((criterium el) (cons #t (filter-list-tf rest criterium)))
                        (else (cons #f (filter-list-tf rest criterium)))))))))

;;; gives the length of the list l
(define length 
    (lambda (l) 
        (cond ((or (not (list? l)) (null? l)) 0)
              (else
                 (+ 1 (length (cdr l)))))))


;;; Choses a pivot for a list
;;; This is done by selecting a random element 
;;; Guile-Specific: random
;;; Guile-2.0 will support SRFI-27: random-integer
(define get-pivot 
  (lambda (l)
    (let* ((len (length l)))
      (cond ((< len 1) #f)
            (else (get-nth l (random len)))))))
      

(define get-filtered-lists 
  (lambda (l criterium) 
    (cond ((not (list? l)) '())
          (else
            (let ((to-sort (car l)) (track (cdr l)))
              (cond ((or (not (list? to-sort)) (not (list? track))) #f)
                    ((or (null? to-sort) (null? track)) (cons '() '()))
                    (else (let* ((next-el (car to-sort)) (next-track (car track)) 
                                 (rest-el (cdr to-sort)) (rest-track (cdr track))
                                 (fullfilled (criterium next-el)))
;                                 (rest-result (get-filtered-lists (cons rest-el rest-track)) criterium)
;                                 (rest-result-el (cond ((list? l) (car rest-result))
;                                                      (else '())))
;                                 (rest-result-track (cond ((list? l) (cdr rest-result))
;                                                           (else '())))
                                 (cond ((eq? fullfilled #t) 
                                        (cons (cons next-el (car (get-filtered-lists (cons rest-el rest-track) criterium)))
					      (cons next-track (cdr (get-filtered-lists (cons rest-el rest-track) criterium)))))
                                       (else 
                                        (cons rest-result-el rest-result-track))))))))))
  
                    

(define get-sorting-permutation
    (lambda (l)
        (cond ((or (not (list? l)) (null? l)) #f)
              (else
                (let ((to-sort (car l)) (track (cdr l)))
                  (cond ((or (not (list? to-sort)) (null? to-sort)) '()) 
                        (else
                          ((let ((pivot (get-pivot to-sort)))
                            ())))))))))
             

