#lang racket


(define (f x)
  (let ([x (+ x x)]
        [y (+ x x)])
    (+ x y)))

(define (g x)
  (let* ([x (+ x x)]
         [y (+ x x)])
    (+ x y)))

(define (h x)
  (let* ([y (+ x x)]
         [x (+ x x)])
    (+ x y)))

(define p1 (cons 1 "hola"))

(define l1 (cons "a" (cons 1 (cons 'hola (cons #f '())))))

;; no es una lista si no termina con '()
(define l2 (cons "a" (cons 1 (cons 'hola (cons #f #t)))))

#|
(list "a" 1 'hola #f)
'("a" 1 'hola #f)
'()
(list)
null
empty
|#

(define v1 (vector "a" 1 'hola #f))
(define q1 '(1 2 (+ 1 2)))

(define (add1 n)
  (+ n 1))

(define l3 '(1 2 3 4 5 6))

(define (p x) (+ x 1))

(define pp (λ (x) (+ x 1)))

(define ppp (lambda (x) (+ x 1)))

(let ([f (λ (x) (+ 1 x))])
  (f 1))

(define (addn n)
  (λ (x)
    (+ x n)))

#|
(define (addn2 n)
  (define (f x)
    (+ x n))
  "return f"
|#
  

(define add2 (addn 2))

(define add124 (addn 124))
































