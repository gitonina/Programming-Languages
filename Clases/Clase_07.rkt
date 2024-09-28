#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x))

#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
|#
;; parse :: <s-expr> -> Expr
;; Parses arithmetical language.
(define (parse s-expr)
  (match s-expr
    ;[n #:when (number? n) (num n)]
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (with x (parse named-expr) (parse body))]))

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
               (subst b what for)))]))  

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

;; ... los ejemplos de las slides se pueden usar como test ...

;; interp :: Expr (AST) -> number
;; Evaluates an arithmetic expression.
(define (interp expr)
  (match expr
    [(num n) n]
    [(add l-expr r-expr) (+ (interp l-expr) (interp r-expr))]
    [(sub l-expr r-expr) (- (interp l-expr) (interp r-expr))]
    [(if0 c-expr t-expr f-expr) (if (zero? (interp c-expr))
                                    (interp t-expr)
                                    (interp f-expr))]
    [(with x named-expr body)
     (interp (subst body x (num (interp named-expr))))]
    [(id x) (error 'interp "Open expression (free occurrence of ~a)" x)]))

(test (interp (num 1)) 1)
(test (interp (num 14)) 14)
(test (interp (add (num 1) (num 2))) 3)
(test (interp (sub (num 2) (num 1))) 1)
(test (interp (add (sub (num 3) (num 4)) (add (num 5) (num 9)))) 13)

(test (interp (parse '1)) 1)
(test (interp (parse '14)) 14)
(test (interp (parse '{+ 1 2})) 3)
(test (interp (parse '{- 2 1})) 1)
(test (interp (parse '{+ {- 3 4} {+ 5 9}})) 13)

(test (interp (parse '{with {x 0} {with {x 1} x}})) 1)
(test (interp (parse '{with {x 5} {+ x x}})) 10)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (interp (parse '{with {x 10} {with {x 1} x}})) 1)
(test (interp (parse '{with {x 10} {with {x x} {+ x x}}})) 20)
(test (interp (parse '{with {x 5} {with {y x} y}})) 5)
(test (interp (parse '{with {x 5} {+ x {with {x 3} {+ x x}}}})) 11)
(test (interp (parse '{with {x 5} {+ x {with {y 3} {+ y x}}}})) 13)
(test (interp (parse '{with {x 5}
                            {+ {with {x 10} {+ x x}}
                               {with {y {+ x x}} {+ y x}}}}))
      35)
(test (interp (parse '{with {x {+ 5 5}}
                            {with {y {- x 3}}
                                  {with {x {+ y x}}
                                        {with {z {+  x y}}
                                              {with {x z}
                                                    {+ x y}}}}}}))
      31)


