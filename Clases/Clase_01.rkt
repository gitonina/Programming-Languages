#lang racket


(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

;; cuadratic :: Number Number Number -> Number
;; calcula el valor ax^2+b
(define (cuadratic x a b)
  (+ (* a (square x) b)))

;; square :: Number -> Number
;; duplica el valor de un número
(define (square x)
  (* x x))

;; linear :: Number Number Number -> Number
;; calcula el valor ax+b
(define (linear x a b)
  (+ (* a x) b))

(define (solve-cuadratic-no-let a b c)
  (if (>= (- (square b) (* 4 a c)) 0)
      (+ (- b) (/ (sqrt (- (square b) (* 4 a c))) (* 2 a)))
      (error "No real solution")))

(define (solve-cuadratic-let a b c)
  (let ([discriminant (- (square b) (* 4 a c))])
    (if (>= discriminant 0)
        (+ (- b) (/ (sqrt discriminant) (* 2 a)))
        (error "No real solution"))))

(define (discriminant a b c)
  (- (square b) (* 4 a c)))

(define (solve-cuadratic-2 a b c)
  ;;(let ([d (discriminant a b c)])
  ;;  ...
  (if (>= (discriminant a b c) 0)
      (+ (- b) (/ (sqrt (discriminant a b c))) (* 2 a))
      (error "error...")))

(let ([x 1])
    (let ([y 2])
      (+ x y)))

(define (my-max a b)
  (if (>= a b)
      a
      b))

(define (pick-random a b)
  (if (>= (random) 0.5)
      a
      b))

(define (pick-random-in-interval a b)
  (let ([rango (- b a)])
    (+ (* (random) rango) a)))

(define par1 (cons #t "hola"))

