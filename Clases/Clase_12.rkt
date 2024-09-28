#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <sym>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun x b)
  (app f arg))

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (app (fun x (parse body)) (parse named-expr))]
    [(list 'fun (list (? symbol? x)) body) (fun x (parse body))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))     

;; Values of expressions
(deftype Value
  (numV n)
  (closureV id body env)
  ;; call-by name: without cache
  ;; (exprV expr env)
  ;; call-by need: with cache
  (exprV expr env cache))

(define (numV+ v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (+ n1 n2)))

(define (numV- v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (- n1 n2)))

(define (numV-zero? v)
  (def (numV n) v)
  (zero? n))


;; Environment
(deftype Env
  (mtEnv)
  (aEnv x v env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error "free identifier ~a" x)]
    [(aEnv y v e)
     (if (symbol=? x y)
         v
         (env-lookup x e))]))

;; strict :: Value -> Value [without exprV]
;; Further reduces a Value to a numV or closureV
;; call-by name: without cache
#|
(define (strict v)
  (match v
    [(exprV expr env) (strict (interp expr env))]
    [_ v]))
|#

;; strict :: Value -> Value [without exprV]
;; Further reduces a Value to a numV or closureV
;; call-by need: with cache
(define (strict v)
  (match v
    [(exprV expr env cache)
     (if (not (unbox cache)) ;; if expr hasn't been reduced so far...
         (let ([val (strict (interp expr env))]) ;; reduce it
           (set-box! cache val) ;; cache its value
           val) ;; return this value
         (unbox cache))]
    [_ v]))


;; interp :: Expr Env -> Value
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (numV+ (strict (interp l env)) (strict (interp r env)))]
    [(sub l r) (numV- (strict (interp l env)) (strict (interp r env)))]
    [(if0 c t f) (if (numV-zero? (strict (interp c env)))
                     (interp t env)
                     (interp f env))]
    [(app f e)
     (def (closureV the-arg the-body closed-env) (strict (interp f env)))
     ;; Eager evaluation
     ;; (def new-env (extend-env the-arg *(interp e env)* closed-env))
     ;; Lazy evaluation
     ;; call-by name: without cache
     ;; (def new-env (extend-env the-arg (exprV e env) closed-env))
     ;; call-by need: with cache
     (def new-env (extend-env the-arg (exprV e env (box #f)) closed-env))
     (interp the-body new-env)]))

;; run
(define (run s-expr)
  (strict (interp (parse s-expr) (empty-env))))

(test (run '1) (numV 1))
(test (run '{+ 1 1}) (numV 2))
;(test (run '{{fun {x} x} 3}) (exprV (num 3) (empty-env)))
(test (run '{{fun {x} x} 3}) (numV 3))
(test (run '{+ {{fun {x} x} 3} 4}) (numV 7))
(test (run '{{fun {x} {+ x x}} 3}) (numV 6))
(test (run '{{fun {x} x} {fun {y} y}}) (closureV 'y (id 'y) (mtEnv)))



