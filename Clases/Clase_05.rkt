#lang play

(print-only-errors #t)

#|
<BinTree> ::= (leaf <value>)
            | (in-node <value> <BinTree> <BinTree>)
|#
(deftype BinTree
  (leaf value)
  (in-node value left right))

(define bt1 (leaf 0))
(define bt2 (in-node 1 (leaf 1) (in-node 5 (leaf 0) (leaf 3))))

;; height :: BinTree -> number
;; Computes height of a binary tree.
(define (height bt)
  (match bt
    [(leaf v) 0]
    [(in-node v l r) (+ 1 (max (height l) (height r)))]))

(test (height bt1) 0)
(test (height bt2) 2)


;; sum-bin-tree :: BinTree -> number
;; Computes the sum of all numeric values in the binary tree.
(define (sum-bin-tree bt)
  (match bt
    [(leaf v) v]
    [(in-node v l r) (+ v (sum-bin-tree l) (sum-bin-tree r))]))

(test (sum-bin-tree bt1) 0)
(test (sum-bin-tree bt2) 10)

;; max-bin-tree :: BinTree -> number
;; Returns the maximum value in the binary tree.
(define (max-bin-tree bt)
  (match bt
    [(leaf v) v]
    [(in-node v l r) (max v (max-bin-tree l) (max-bin-tree r))]))

(test (max-bin-tree bt1) 0)
(test (max-bin-tree bt2) 5)
    
;; fold-bintree :: (f-leaf :: Number -> A) (f-nodes :: Number A A -> A) -> (BinTree -> A) 
;; Fold over numeric binary trees.
(define (fold-bintree f-leaf f-nodes)
  (λ (bt)
    (match bt
      [(leaf v) (f-leaf v)]
      [(in-node v l r)
       (f-nodes v
                ((fold-bintree f-leaf f-nodes) l)
                ((fold-bintree f-leaf f-nodes) r))])))

;; Utilice fold para definir ...
;; fold-height --> A pasa a ser Number
(define fold-height
  (fold-bintree (λ (v) 0)
                (λ (v lv rv) (+ 1 (max lv rv)))))
(test (fold-height bt1) 0)
(test (fold-height bt2) 2)

(define fold-sum-bin-tree
  (fold-bintree (λ (v) v)
                (λ (v lv rv) (+ v lv rv))))
(test (fold-sum-bin-tree bt1) 0)
(test (fold-sum-bin-tree bt2) 10)

(define fold-max-bin-tree
  (fold-bintree (λ (v) v)
                (λ (v lv rv) (max v lv rv))))
(test (fold-max-bin-tree bt1) 0)
(test (fold-max-bin-tree bt2) 5)





#|
Gramática abstracta:
<expr> ::= (num <num>)
         | (add <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r))


#|
Gramática concreta:
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
|#

;; parse :: s-expr -> Expr
;; Parses source code to Expr AST.
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [(list '+ l-expr r-expr)
     (add (parse l-expr) (parse r-expr))]))

(test (parse '1) (num 1))
(test (parse '14) (num 14))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ {+ 1 1} 3}) (add (add (num 1) (num 1)) (num 3)))
(test (parse '{+ 3 {+ 1 1}}) (add (num 3) (add (num 1) (num 1))))





