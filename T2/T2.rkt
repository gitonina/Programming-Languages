#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO) NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
<prop> ::= (tt)
         | (ff)
         | (p-id <sym>)
         | (p-not <prop>)
         | (p-and <prop>)
         | (p-or <prop>)
         | (p-where <prop> <id> <prop>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-not n)
  (p-and a)
  (p-or o)
  (p-where p i q)
  (p-id s)
  )


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          |<sym>
          |(list 'not <s-prop>)
          |(list 'and <s-prop>)
          |(list 'or <s-prop>)
          |(list '<s-prop> where(list <sym> <s-prop>)) 
|#

;; parse-prop : <s-prop> -> Prop
(define (parse-prop s-expr)
   (match s-expr
     ['true (tt)]      
     ['false (ff)]
     [(? symbol? x) (p-id x) ]
     [(list 'and formula ...)
       (if (< (length formula) 2)
         (error 'parse-prop "and expects at least two operands")
         (p-and (map parse-prop formula)))] 

     [(list 'or formula ...)   (if (< (length formula) 2)
         (error 'parse-prop "or expects at least two operands")
         (p-or (map parse-prop formula)))]
     
     [(list 'not formula) (p-not  (parse-prop formula))]
     [(list formula 'where (list id expr)) (p-where (parse-prop formula) id (parse-prop expr))]
     )
  )


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<pvalue> ::= (ttV)
           | (ffV)
|#
(deftype PValue
  (ttV)
  (ffV))



;; from-Pvalue : PValue -> Prop
(define (from-PValue pv)
  (match pv
    [(ttV) (tt)]
    [(ffV) (ff)]))



;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id s) (if (eq? s name) substitution target)]
    [(p-not n) (p-not (p-subst n name substitution))]
    [(p-and a) (p-and (map (lambda (p) (p-subst p name substitution)) a))]
    [(p-or o) (p-or (map (lambda (p) (p-subst p name substitution)) o))]
    [(p-where p i q) 
     (if (eq? i name)
         (p-where p i (p-subst q name substitution))
         (p-where (p-subst p name substitution) i (p-subst q name substitution)))]))


;;----- ;;
;; P1.e ;;
;;----- ;;



;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps)
  (match ps
    ['() (ffV)] 
    [(cons p rest)
     (let ([p-value (p-eval p)])
       (if (equal? p-value (ttV))
           (ttV) 
           (eval-or rest)))]))


;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps)
  (match ps
    ['() (ttV)]  
    [(cons p rest)
     (let ([p-value (p-eval p)])
       (if (equal? p-value (ffV))
           (ffV) 
           (eval-and rest)))]))

;; p-eval : Prop -> PValue
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-not n) (if (equal? (p-eval n) (ttV)) (ffV) (ttV))]
    [(p-and a) (eval-and a)]
    [(p-or o) (eval-or o)]
    [(p-where p i q) 
     (let ([q-value (p-eval q)])
       (p-eval (p-subst p i (from-PValue q-value))))]
    [(p-id s) (error (format "Unbound variable: ~a" s))]))


;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;

#|
<expr> ::= (real <num>)
        | (id <sym>)
        | (imaginary <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (with [((<sym> <expr>))*] <expr>)
|#
(deftype Expr
  (real r)
  (imaginary i)
  (id x)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with bindings body)
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (cons <num> i)
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | (list 'with (list <sym> <s-expr>) <s-expr>)
        | <sym>
|#

;; parse : <s-expr> -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (real s-expr)]  
    [(list n i) (imaginary n)]  
    [( list '+ l r) (add (parse l) (parse r))] 
    [(list '- l r) (sub (parse l) (parse r))]  
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))] 
    [(list 'with bindings body)
     (if (null? bindings)
         (error "parse: 'with' expects at least one definition")
         (let* ([parsed-bindings (map (lambda (b) 
                                        (match b
                                          [(list id expr) (cons id (parse expr))]
                                          [_ (error "Invalid binding in 'with'")]))
                                      bindings)])
           (with parsed-bindings (parse body))))]

    [(? symbol?) (id s-expr)] 
    [_ (error "Invalid expression" s-expr)])) 



;;----- ;;
;; P2.c ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v)
  (match v
      [(compV r i)
       (cond [(= i 0) (real r)]
             [(= r 0) (imaginary )]
             [else (add (real r) (imaginary i) )])]))



;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2)
       (match (list v1 v2)
         [(list (compV r1 i1) (compV r2 i2))   (compV (+ r1 r2) (+ i1 i2)) ]))

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2)
       (match (list v1 v2)
         [(list (compV r1 i1) (compV r2 i2))   (compV (- r1 r2) (- i1 i2)) ]))


;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v)
  (match v
     [(compV r i)
    (if ( and (= r 0) (= i 0)) #t #f) ]))



;;----- ;;
;; P2.d ;;
;;----- ;;


;; aux-subst :: [((<sym> <expr>))*] Symbol Expr -> [((<sym> <expr>))*]
(define (aux-subst bindings what for)
  (match bindings
    ['() '()]
    [(cons (cons id val) rest-bindings)
     (if (shadowing? bindings what)
         (cons (cons id val) rest-bindings)
         (cons (cons id (subst val what for))
               (aux-subst rest-bindings what for)))]))


;; shadowing? :: [((<sym> <expr>))*] Symbol  -> Boolean
(define (shadowing? bindings what)
  (match bindings
    ['() #f]
    [(cons (cons id _) rest-bindings)
     (if (eq? id what)
         #t
         (shadowing? rest-bindings what))]))



;; subst :: Expr Symbol Expr -> Expr
(define (subst expr what for)
  (match expr
    [(real r) (real r)]
    [(imaginary i) (imaginary i)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(id x)
     (if (eq? x what)
         for
         (id x))]
    [(with bindings body)
     (let ([new-bindings (aux-subst bindings what for)])
       (with new-bindings
             (if (shadowing? new-bindings what)
                 body  
                 (subst body what for))))]))


                                          
;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr)
    (match expr
    [(real r) (compV r 0)]
    [(imaginary i) (compV 0 i)]
    [(id x) (error 'interp "Open expression (free occurrence of ~a)" x)]
    [(add l-expr r-expr) (cmplx+ (interp l-expr) (interp r-expr))]
    [(sub l-expr r-expr) (cmplx- (interp l-expr) (interp r-expr))]
    [(if0 c-expr t-expr f-expr)  (if (cmplx0? (interp c-expr))
         (interp t-expr)
         (interp f-expr))]
    [(with bindings body)
          (interp  (from-CValue (fold-bindings bindings body)))]))
  
  
(define (fold-bindings bindings body)
  (match bindings
    ['() body]
    [(cons (cons id expr) rest-bindings)
     (let ([new-body (subst body id (interp expr))])
       (fold-bindings rest-bindings new-body))]))
