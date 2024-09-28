#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         ;;| (bool <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
|#
(deftype Expr
  (num n)
  ;; (bool b)
  (add l r)
  (sub l r)
  (if0 c t f))

#|
<s-expr> ::= <num>
           ;; | <bool>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
|#
;; parse :: <s-expr> -> Expr
;; Parses arithmetical language.
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    ;; [b #:when (boolean? b) (bool b)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]))

(test (parse '1) (num 1))
(test (parse '14) (num 14))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ {+ 1 3} 2}) (add (add (num 1) (num 3)) (num 2)))
(test (parse '{+ 2 {+ 1 3}}) (add (num 2) (add (num 1) (num 3))))

(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{- {+ 1 2} {- 2 3}}) (sub (add (num 1) (num 2)) (sub (num 2) (num 3))))

(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{if0 {+ 1 1} 1 2}) (if0 (add (num 1) (num 1)) (num 1) (num 2)))

;;(test (parse '#f) (bool #f))
;;(test (parse '#t) (bool #t))

;; calc :: Expr (AST) -> number
;; Evaluates an arithmetic expression.
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l-expr r-expr) (+ (calc l-expr) (calc r-expr))]
    [(sub l-expr r-expr) (- (calc l-expr) (calc r-expr))]
    [(if0 c-expr t-expr f-expr) (if (zero? (calc c-expr))
                                    (calc t-expr)
                                    (calc f-expr))]))  

(test (calc (num 1)) 1)
(test (calc (num 14)) 14)
(test (calc (add (num 1) (num 2))) 3)
(test (calc (sub (num 2) (num 1))) 1)
(test (calc (add (sub (num 3) (num 4)) (add (num 5) (num 9)))) 13)

(test (calc (parse '1)) 1)
(test (calc (parse '14)) 14)
(test (calc (parse '{+ 1 2})) 3)
(test (calc (parse '{- 2 1})) 1)
(test (calc (parse '{+ {- 3 4} {+ 5 9}})) 13)










