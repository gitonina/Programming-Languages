#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
         | (app <sym> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x)
  (app f-name f-arg))

#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
           | (list <sym> <s-expr>) <-- application
|#
;; parse :: <s-expr> -> Expr
;; Parses arithmetical language.
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (with x (parse named-expr) (parse body))]
    [(list f-name f-arg) (app f-name (parse f-arg))]))

(test (parse '1) (num 1))
(test (parse '14) (num 14))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ {+ 1 3} 2}) (add (add (num 1) (num 3)) (num 2)))
(test (parse '{+ 2 {+ 1 3}}) (add (num 2) (add (num 1) (num 3))))

(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{- {+ 1 2} {- 2 3}}) (sub (add (num 1) (num 2)) (sub (num 2) (num 3))))

(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{if0 {+ 1 1} 1 2}) (if0 (add (num 1) (num 1)) (num 1) (num 2)))

(test (parse 'x) (id 'x))
(test (parse '{with {x {+ 5 5}} {+ x x}})
      (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))

(test (parse '{double 1}) (app 'double (num 1)))


;; subst :: Expr Symbol Expr -> Expr
;; (subst in what for)
;; substituye todas las ocurrencias libres del identificador 'what'
;; en la expresión 'in' por la expresión 'for'
;; ....
;; subst x->5 in {+ x x} ---> (subst {+ x x} 'x (num 5))
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(id x)
     (if (symbol=? x what)
         for
         (id x))]
    [(with x e b)
     (with x
           (subst e what for)
           (if (symbol=? x what)
               b
               (subst b what for)))]
    [(app f-name f-arg) (app f-name (subst f-arg what for))]))

(test (subst (parse 'x) 'x (num 1)) (num 1))
(test (subst (parse 'y) 'x (num 1)) (id 'y))
(test (subst (parse '{+ x 1}) 'x (num 5)) (add (num 5) (num 1)))
;; ... otros tests ...
(test (subst (parse '{with {x 1} {+ x x}}) 'x (num 5))
      (parse '{with {x 1} {+ x x}}))
(test (subst (parse '{with {y 1} {+ x x}}) 'x (num 5))
      (parse '{with {y 1} {+ 5 5}}))
(test (subst (parse '{with {y x} {+ x x}}) 'x (num 5))
      (parse '{with {y 5} {+ 5 5}}))


;; Function definitions
;; <fundef> ::= (fundef <sym> <sym> <expr>)
(deftype FunDef
  (fundef name arg body))

;; lookup :: symbol listof(FunDef) -> FunDef
;; Searches a function definition within a list of definitions.
(define (lookup f-name defs)
  (match defs
    [(list) (error 'lookup "Function ~a not found" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                          head
                          (lookup f-name tail))]))

;; <env> ::= mtEnv
;;         | (aEnv <sym> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; empty-env :: Env
(define empty-env (mtEnv))

;; extend-env :: Symbol Value Env -> Env
(define extend-env aEnv)

;; env-lookup :: Symbol Env -> Value
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? x id)
                            val
                            (env-lookup x rest))]))

;; interp :: Expr listof(FunDef) Env -> number
;; Evaluates an arithmetic expression.
(define (interp expr f-list env)
  (match expr
    [(num n) n]
    [(add l-expr r-expr) (+ (interp l-expr f-list env) (interp r-expr f-list env))]
    [(sub l-expr r-expr) (- (interp l-expr f-list env) (interp r-expr f-list env))]
    [(if0 c-expr t-expr f-expr) (if (zero? (interp c-expr f-list env))
                                    (interp t-expr f-list env)
                                    (interp f-expr f-list env))]
    
    [(with x named-expr body)
     (def new-env (extend-env x (interp named-expr f-list env) env))
     (interp body f-list new-env)]

    [(id x) (env-lookup x env)]
    
    [(app f-name f-arg)
     (def (fundef _ the-arg the-body) (lookup f-name f-list))
     (def new-env (extend-env the-arg (interp f-arg f-list env) empty-env))     
     (interp the-body f-list new-env)]))

(test (interp (num 1) '() empty-env) 1)
(test (interp (num 14) '() empty-env) 14)
(test (interp (add (num 1) (num 2)) '() empty-env) 3)
(test (interp (sub (num 2) (num 1)) '() empty-env) 1)
(test (interp (add (sub (num 3) (num 4)) (add (num 5) (num 9))) '() empty-env) 13)

(test (interp (parse '1) '() empty-env) 1)
(test (interp (parse '14) '() empty-env) 14)
(test (interp (parse '{+ 1 2}) '() empty-env) 3)
(test (interp (parse '{- 2 1}) '() empty-env) 1)
(test (interp (parse '{+ {- 3 4} {+ 5 9}}) '() empty-env) 13)

(test (interp (parse '{with {x 0} {with {x 1} x}}) '() empty-env) 1)
(test (interp (parse '{with {x 5} {+ x x}}) '() empty-env) 10)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '() empty-env) 20)
(test (interp (parse '{with {x 10} {with {x 1} x}})'() empty-env) 1)
(test (interp (parse '{with {x 10} {with {x x} {+ x x}}})'() empty-env) 20)
(test (interp (parse '{with {x 5} {with {y x} y}})'() empty-env) 5)
(test (interp (parse '{with {x 5} {+ x {with {x 3} {+ x x}}}})'() empty-env) 11)
(test (interp (parse '{with {x 5} {+ x {with {y 3} {+ y x}}}})'() empty-env) 13)
(test (interp (parse '{with {x 5}
                            {+ {with {x 10} {+ x x}}
                               {with {y {+ x x}} {+ y x}}}}) '() empty-env)
      35)
(test (interp (parse '{with {x {+ 5 5}}
                            {with {y {- x 3}}
                                  {with {x {+ y x}}
                                        {with {z {+  x y}}
                                              {with {x z}
                                                    {+ x y}}}}}}) '() empty-env)
      31)


;; f(n) = n + n
;; f(4) = 4 + 4
;; aplicación es a través de la substitución...
(define p '{+ 1 {double {+ 1 1}}})
(define fun-double (fundef 'double 'n (parse '{+ n n})))
(define fun-constante (fundef 'double 'x (parse '1)))

;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (interp (parse prog) f-list empty-env))

(test (run '{+ 1 {double 2}} (list fun-double)) 5)

(test/exn (run '{with {n 5} {f 10}} (list (fundef 'f 'x (id 'n)))) "free identifier")

