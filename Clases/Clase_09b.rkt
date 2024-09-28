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
  (fun arg body)
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
     (app (fun x (parse body)) (parse named-expr))]
    [(list 'fun (list (? symbol? x)) b)
     (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))


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
(test (parse '{fun {x} {+ x x}}) (fun 'x (add (id 'x) (id 'x))))
(test (parse '{f 5}) (app (id 'f) (num 5)))
(test (parse '(f 5)) (app (id 'f) (num 5)))
(test (parse '{with {x {+ 5 5}} {+ x x}})
      (app (fun 'x (add (id 'x) (id 'x))) (add (num 5) (num 5))))

;; {func {params x} {+ x x}} --> (fun 'x (add (id 'x) (id 'x)))
;; {defun {+ x x} for x} --> (fun 'x (add (id 'x) (id 'x)))


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
(define (interp expr env)
  (match expr
    [(num n) n]
    [(add l-expr r-expr) (+ (interp l-expr env) (interp r-expr env))]
    [(sub l-expr r-expr) (- (interp l-expr env) (interp r-expr env))]
    [(if0 c-expr t-expr f-expr) (if (zero? (interp c-expr env))
                                    (interp t-expr env)
                                    (interp f-expr env))]
    [(id x) (env-lookup x env)]
    [(fun arg body) (fun arg body)]))

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

;; run :: s-expr listof(FunDef) -> number
(define (run prog)
  (interp (parse prog) empty-env))
