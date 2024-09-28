#lang play

;; switch/case

(define (f v)
  (match v
    ["hola" (print "v es el string 'hola'")]
    [23 (print "v es el número 23")]
    ;;[else (print "no es nada conocido")]))
    [_ (print "no es nada conocido")]))

(define y #f)
;; el lenguaje de patrones es "composable"
(define (match-pair v)
  (match v
    [(cons 1 2) (printf "es exactamente el pair (1 . 2) ~a" y)]
    [(cons 14 27) (print "es exactamente el pair (14 . 27)")]
    [(cons -100 #f) (print "es exactamente el pair (-100 . #f)")]
    ;; pattern variables
    [(cons 1 y) (printf ">> es el pair (1 . ~a)" y)]
    [(cons 1 (cons "hola" 'f)) (print "es par anidado")]
    [_ (print "es otro par")]))

#|
(define (match-pair-2 v)
  ;; hay que ocupar los accesores de la estructura de datos...
  (let ([a (car v)]
        [b (cdr v)])
    (cond
      [(and (= a 1) (= b 2)) (printf "es exactamente el pair (1 . 2) ~a" y)]
      [(and (= a 14) (= b 27)) (print "es exactamente el pair (14 . 27)")]
      [(and (= a -100) (not b)) (print "es exactamente el pair (-100 . #f)")]
      [(= a 1) (printf ">> es el pair (1 . ~a)" b)]
      [(and (= a 1) (pair? b))
       (let ([aa (car b)]
             [bb (cdr b)])
         (when(and (equal? "hola" aa) (equal? 'f bb))
             (print "es par anidado")))]             
      [else (print "es otro par")])))
|#

;; fff :: (Pair Number String) -> Symbol <---- contrato
;; Calcula ....
(define (fff p)
  'hola)

(define (my-length-cons l)
  (match l
    ['() 0]
    ;; 'rest' es una lista
    [(cons first rest) (+ 1 (my-length-cons rest))]))

(define (my-length-list l)
  (match l
    ['() 0]
    ;; [(list first rest) (printf "first: ~a y rest: ~a" first rest)]))
    [(list f r ...) (printf "first: ~a y rest: ~a" f r)]))

(define (match-binop l)
  (match l
    [(list '+ a b) (printf "sum of ~a and ~a" a b)]
    [(list '- a b) (printf "difference of ~a and ~a" a b)]
    [(list '* a b) (printf "product of ~a and ~a" a b)]))


(define p1 '{+ 1 2})


(define (md l)
  (match-define (cons a b) l)
  (printf "first: ~a and rest: ~a" a b))











