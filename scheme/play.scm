guile> (define get-nth (lambda (x n) (
           cond ((or (< n 0) (not (cons? x))) #f)
                (else 
                 (cond ((eq x '()) #f)
                       ((= n 0) (car x))
                       (else (get-nth x (- n 1)))))))) 
guile> (get-nth '(1 2 3 4 5) 1)

Backtrace:
In standard input:
   7: 0* [get-nth (1 2 3 4 5) 1]
   1: 1  (cond ((< n 0) #f) ((cons? x) (cond (# #f) (# #) (else #))))
   3: 2* (cons? x)

standard input:3:18: In expression (cons? x):
standard input:3:18: Unbound variable: cons?
ABORT: (unbound-variable)
guile> (define get-nth (lambda (x n) (
           cond ((or (< n 0) (not (list? x))) #f)
                (else 
                 (cond ((eq x '()) #f)
                       ((= n 0) (car x))
                       (else (get-nth x (- n 1)))))))) 
guile> (get-nth '(1 2 3 4 5) 1)

Backtrace:
In standard input:
  14: 0* [get-nth (1 2 3 4 5) 1]
   8: 1  (cond ((or (< n 0) (not #)) #f) (else (cond (# #f) (# #) (else #))))
  11: 2  (cond ((eq x (quote ())) #f) ((= n 0) (car x)) (else (get-nth x (- n 1))))
  11: 3* (eq x (quote ()))

standard input:11:25: In expression (eq x (quote ())):
standard input:11:25: Unbound variable: eq
ABORT: (unbound-variable)
guile> (define get-nth (lambda (x n) (
           cond ((or (< n 0) (not (list? x))) #f)
                (else 
                 (cond ((null? x) #f)
                       ((= n 0) (car x))
                       (else (get-nth x (- n 1)))))))) 
guile> (get-nth '(1 2 3 4 5) 1)
1
guile> (get-nth '(1 2 3 4 5) 2)
1
guile> (define get-nth (lambda (x n) (
           cond ((or (< n 0) (not (list? x))) #f)
                (else 
                 (cond ((null? x) #f)
                       ((= n 0) (car x))
                       (else (get-nth (cdr x) (- n 1)))))))) 