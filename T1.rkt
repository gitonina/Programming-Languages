#lang play
(require math/flonum)
;;Parte a

#|
 <CFraction> ::= (simple <Integer>)
             |   (compound <Integer> <Integer> <CFraction>
|#

(deftype CFraction
  (simple value)
  (compound value1 value2 fraccion))



;;Parte b

 ;;eval :: CFraction -> Rational
 ;;Evalua una fracción continua, devolviendo el numero racional que representa.

(define (eval cf)
  (match cf
    [(simple a) a]
    [(compound a b d)
     (+ a (/ b (eval d)))]))


;;Parte c

  ;;degree :: Cfraction -> Integer
  ;;Devuelve el grado de una fracción continua

 (define (degree cf)
  (match cf
    [(simple _) 0]
    [(compound _ _ d) (+ 1 (degree d))]))


;; Parte d
   ;; fold-cfraction :: (Integer -> A) (Integer Integer A ->A) -> (CFraction -> A)
   ;; Captura el esquema de recursión asociado a CFraction

  (define (fold-cfraction  f-simple f-compound)
    (λ(cf)
      (match cf
          [(simple a) (f-simple a)]
          [(compound a b d)
           (f-compound a b ((fold-cfraction f-simple f-compound) d))])))

;;Parte e

    ;; fold-eval :: CFraction -> Rational
    ;; Evalúa una fracción continua y devuelve su valor como un número racional
   (define fold-eval
     (fold-cfraction(λ(a) a)
                    (λ(aa bb dd) (+ aa (/ bb dd)))))


    ;; fold-degree :: CFraction -> Integer
    ;; Calcula el grado de una fracción continua
   (define fold-degree
     (fold-cfraction(λ (_) 0)
                    (λ (_ _2 d) (+ 1 d))))
  

;;Parte f

;;Se define h que sirve para la recursión
(define h 2)

;; mysterious-cf :: Integer -> CFraction
;; Genera una secuencia de fracciones continuas según el patrón especificado
;; Lanza un error si el argumento es negativo
(define (mysterious-cf n)
  (if (negative? n)
      (error "Error: argumento negativo")
      (mysterious-cf-aux1 n)))


;; mysterious-cf-aux1 :: Integer -> CFraction
;; Función auxiliar que maneja el caso base y la estructura inicial de la fracción continua
(define (mysterious-cf-aux1 n)
  (if (zero? n)
      (simple 6)
      (compound 6 1 (mysterious-cf-aux2 (+ n 1) h ))))


;; mysterious-cf-aux2 :: Integer Integer -> CFraction
;; Función auxiliar que construye recursivamente la estructura interna de la fracción continua
;; k: límite superior para a (normalmente n+1)
;; a: contador que se incrementa en cada llamada recursiva, comienza en 2
(define (mysterious-cf-aux2 k a)
  (if (= a k)
      (simple 6)
      (compound 6 (sqr (- (* 2 a) 1)) (mysterious-cf-aux2 k (+ a 1)))))


;;Parte g

;; from-to :: Integer Integer -> ListOf Integer
;; Construye una lista de enteros comprendidos entre dos enteros dados
(define (from-to start end)
  (if (> start end)
      '()
      (cons start (from-to (+ start 1) end))))

;; mysterious-list :: Integer -> ListOf Float
;;Devuelve una lista tal que el i-escimo elemento es calculado como la resta de la evaluación
;;de (mysterious-cf ) menos 3.
(define (mysterious-list n)
  (map (λ (i) (fl (- (eval (mysterious-cf i)) 3)))
       (from-to 0 n)))

;;Teniendo en cuenta que para distintos valores de n, mysterious-list n parece estar convergiendo a valores ligeramente mayores de 3, entonces cuando k tiende
;; infinito, (mysterious-cf k) tiende a un valor muy cercano a 3. (quizas pi).

;;Parte h
 ;;rac-to-cf :: Rational -> CFraction
 ;;Transforma un numero racional no-negativo en su representacion en forma de fraccion continua.
(define (rac-to-cf r)
  (define (aux r)
    (let* ([i (floor r)]
           [f (- r i)])
      (if (= f 0)
          (simple i)
          (compound i 1 (aux (/ 1 f))))))
  (aux r))

 
          