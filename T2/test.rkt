#lang play
(require "T2.rkt")

(print-only-errors #t)

;;P1
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(and true false true)) (p-and ( list (tt) ( ff ) (tt))))
(test (parse-prop ' (not true)) (p-not (tt)))
(test (parse-prop ' (or true false )) (p-or ( list (tt) ( ff ))))
(test (parse-prop ' ( false where [x true] )) (p-where (ff) 'x (tt)))
(test (parse-prop ' (x where [x true] )) (p-where (p-id 'x) 'x (tt)) )
(test (parse-prop '(and false (not true) (or true false)))
      (p-and (list (ff) (p-not (tt)) (p-or (list (tt) (ff))))))
(test (parse-prop '(x where [y (not x)]))
      (p-where (p-id 'x) 'y (p-not (p-id 'x))))
(test/exn (parse-prop ' (and true)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop ' (or)) "parse-prop: or expects at least two operands")


(test (from-PValue (ttV))(tt))
(test (from-PValue (ffV))(ff))

(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x ( ff )) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x ( ff )) (p-where (ff) 'y (tt)))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y))) 'x (tt))
      (p-and (list (tt) (p-id 'y))))
(test (p-subst (p-or (list (p-id 'x) (p-not (p-id 'x)))) 'x (ff))
      (p-or (list (ff) (p-not (ff)))))


(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-not (ff))) (ttV))
(test (p-eval (p-not (p-not (tt)))) (ttV) )
(test (p-eval (p-and (list (tt) (tt) (tt)))) (ttV))
(test (p-eval (p-and (list (tt) (ff) (tt)))) (ffV))
(test (p-eval (p-or (list (ff) (ff) (ff)))) (ffV))
(test (p-eval (p-or (list (ff) (tt) (ff)))) (ttV))
(test (p-eval (p-where (p-id 'x) 'x (tt))) (ttV))
(test (p-eval (p-and (list (tt) (p-or (list (ff) (tt))) (p-not (ff))))) (ttV))
(test (p-eval (p-and (list (tt) (p-or (list (ff) (p-and (list (tt) (p-not (ff))))))))) (ttV) )
(test (p-eval (p-and (list (tt)))) (ttV))
(test (p-eval (p-or (list (p-where (p-id 'x) 'x (ff)) (tt))))
      (ttV))
(test/exn (p-eval (p-id 'x)) "Unbound variable: x")
(test/exn (p-eval (p-and (list (tt) (p-id 'x)))) "Unbound variable: x")
(test (p-eval (p-where (p-where (p-or (list (p-id 'x) (p-id 'y))) 'y (ff)) 'x (tt))) (ttV))

;;P2
(test (parse '1) ( real 1))
(test (parse ' (1 i )) (imaginary 1))
(test (parse ' (+ 1 (2 i ))) (add (real 1) (imaginary 2)))
(test (parse ' (with [(x 1) (y 1)] (+ x y)))  (with ( list (cons 'x ( real 1)) (cons 'y ( real 1))) (add (id 'x) (id 'y))) )
(test (parse '(if0 (- 1 1) 5 10))
      (if0 (sub (real 1) (real 1)) (real 5) (real 10)))
(test (parse '(with [(x (+ 1 (2 i))) (y (- x (1 i)))] y))
      (with (list (cons 'x (add (real 1) (imaginary 2)))
                  (cons 'y (sub (id 'x) (imaginary 1))))
            (id 'y)))
(test/exn (parse ' (with [ ] 1)) "parse: 'with' expects at least one definition")
(test/exn  (parse ' (with [(x 1 4) (y 1)] (+ x y))) "Invalid binding in 'with"  )



(test (from-CValue (compV 1 0 )) (real 1))
(test (from-CValue (compV 0 3)) (imaginary 3))
(test (from-CValue(compV 2 2)) (add (real 2) (imaginary 2)))
(test (from-CValue(compV  0 0 )) (real 0))


(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx+ (compV 1 -2) (compV -3 5))(compV -2 3))
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))
(test (cmplx- (compV 4 7) (compV 1 2))(compV 3 5))
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 0.001 0)) #f)
(test (cmplx0? (compV 0 0)) #t)


(test (subst (parse ' (with [(x 2) (y z) ] (+ x z))) 'z ( real 1))  (with ( list (cons 'x ( real 2)) (cons 'y ( real 1))) (add (id 'x) ( real 1))))
(test (subst (parse ' (with [(x 2) (y x)] (+ x x))) 'x ( real 1)) (with ( list (cons 'x ( real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))) )
(test (subst (parse ' (with [(y x) (x 2)] (+ x x))) 'x ( real 1)) (with ( list (cons 'y ( id 'x)) (cons 'x (real 2))) (add (id 'x) (id 'x))) )
(test (subst (parse ' (with [(z 2) (y x)] (+ x x))) 'x ( real 1)) (with ( list (cons 'z ( real 2)) (cons 'y (real 1))) (add (real 1) (real 1))) )



(test (interp (real 5)) (compV 5 0))
(test (interp (parse '(with [(x 3) (y (+ x (2 i)))] (- y x))))(compV 0 2))
(test (interp (parse '(if0 (- (3 i) (3 i)) 10 20)))(compV 10 0))
(test (interp (imaginary 2)) (compV 0 2))
(test (interp (add (real 1) (imaginary 1))) (compV 1 1))
(test (interp (with (list (cons 'x (real 5))) (id 'x)))  (compV 5 0))
(test (interp (with (list (cons 'x (real 5)) (cons 'y (add (id 'x) (real 3)))) (add (id 'x) (id 'y)))) (compV 13 0))
(test (interp(with( list (cons 'x (real 2)) (cons 'y ( add( id 'x) (real 1)))) (add (id 'x) (id 'y)))) (compV 5 0))
(test/exn (interp(with( list (cons 'x (real 2)) (cons 'y ( add( id 'a) (real 1)))) (add (id 'x) (id 'y)))) "interp: Open expression (free occurrence of a)")
(test/exn (interp( id 'x))  "interp: Open expression (free occurrence of x)")