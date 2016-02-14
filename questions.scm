; Some utility functions that you may find useful.
(define (apply-to-all proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (apply-to-all proc (cdr items)))))

(define (cons-all first rests)
    (apply-to-all (lambda (rest) (cons first rest)) rests))

(define (filter f s)
  (if (null? s) s
    (let ((rest (filter f (cdr s))))
      (if (f (car s)) (cons (car s) rest) rest))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Problem 18
;; Turns a list of pairs into a pair of lists
(define (zip pairs)
      (list (apply-to-all car pairs)
        (apply-to-all cadr pairs))
)

(zip '((1 2) (3 4) (5 6)))
; expect ((1 3 5) (2 4 6))
(zip '((1 2)))
; expect ((1) (2))
(zip '())
; expect (() ())

; Problem 19

;; List all ways to partition TOTAL without using consecutive numbers.
(define (list-partitions total)
    (define (partitions n m)
        (cond 
            ((< n 0) nil)
            ((= m 0) nil)
            ((= n 0) (list nil))
            (else 
                (define without_m (partitions n (- m 1)))
                (define using_m (partitions (- n m) m))
                (define with_m (apply-to-all (lambda (s) (cons m s)) using_m))
                (append with_m without_m)
            )
        )
    )
    (filter no_consecutive (partitions total total))
)

(define (no_consecutive s)
    (cond ((null? s) True)
        ((null? (cdr s)) True)
        ((consecutive? (car s) (cdr s)) False)
        (else (no_consecutive (cdr s)))
    )
)

(define (consecutive? x s)
    (cond ((null? s) False)
        ((or (= -1 (- x (car s))) (= 1 (- x (car s)))) True)
        (else (consecutive? x (cdr s)))
    )
)

; For these two tests, any permutation of the right answer will be accepted.
(list-partitions 5)
; expect ((5) (4 1) (3 1 1) (1 1 1 1 1))
(list-partitions 7)
; expect ((7) (6 1) (5 2) (5 1 1) (4 1 1 1) (3 3 1) (3 1 1 1 1) (1 1 1 1 1 1 1))

; Problem 20
;; Returns a function that takes in an expression and checks if it is the special
;; form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
    (cond ((atom? expr) expr)
        ((quoted? expr) expr)
        ((or (lambda? expr) (define? expr)) 
            (let ( (form   (car expr))
                   (params (cadr expr))
                   (body   (cddr expr)))
            (cons form (cons params (apply-to-all analyze body)))
            )
        )
        ((let? expr) (let ((values (cadr expr))
                           (body (cddr expr)))
                      (define a (cdr (zip values)))
                     (cons (cons 'lambda  (cons (car (zip values)) (apply-to-all analyze body))) 
                        (apply-to-all analyze (car a)))
                     )
        )
        (else (apply-to-all analyze expr))
    )
)

;; Lambda parameters not affected, but body affected
(analyze '(lambda (let a b) (+ let a b)))
; expect (lambda (let a b) (+ let a b))
(analyze '(lambda (x) a (let ((a x)) a)))
; expect (lambda (x) a ((lambda (a) a) x))

(analyze '(let ((a 1)
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) 1 2)
(analyze '(let ((a (let ((a 2)) a))
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) ((lambda (a) a) 2) 2)
(analyze '(let ((a 1))
            (let ((b a))
              b)))
; expect ((lambda (a) ((lambda (b) b) a)) 1)
(analyze '(+ 1 (let ((a 1)) a)))
; expect (+ 1 ((lambda (a) a) 1))

(analyze 1)
; expect 1
(analyze 'a)
; expect a
(analyze '(+ 1 2))
; expect (+ 1 2)

;; Quoted expressions remain the same
(analyze '(quote (let ((a 1) (b 2)) (+ a b))))
; expect (quote (let ((a 1) (b 2)) (+ a b)))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  'YOUR-CODE-HERE
  nil)

