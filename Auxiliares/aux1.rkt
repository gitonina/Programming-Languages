#lang play
(print-only-errors #t)
;;Aux 1
;;P1
#|
A) ¿Cual es la diferencia entre (cons 'a 'b ) y ( list 'a 'b')? Escriba el último en
notación de pares.

El primero es un par, donde el primer elemento es el simbolo 'a y el segundo elemento es el simbolo 'b. El segundo es una lista, donde tiene los mismos elementos
pero ademas tiene un ultimo elemento que corresponde a la lista vacia.
|#
(test (list 'a 'b)  (cons 'a (cons 'b '())) )


;;B)Escriba el código que genere la lista ' (1 (x (y . z) #f))


;; Primera forma de escribirlo: (list 1 (list 'x (cons 'y 'z) #f))
;;Segunda forma: (cons '1 (cons ( cons 'x (cons(cons 'y 'z) (cons #f '() ))) '())) 


(test  (list 1 (list 'x (cons 'y 'z) #f))  ' (1 (x (y . z) #f)))
(test   (cons 1( cons (cons 'x (cons (cons 'y 'z) (cons #f '()))) '())) ' (1 (x (y . z) #f)))


;; C) Dado (define l ( list ' (a (b c)) ' (d e f))), ¿Cómo accedería al elemento b y el
;;f en l? Hint: Para acceder al elemento d es: (car (car (cdr l ))).
(define l ( list '(a (b c))  '(d e f) ))
(test    (car(car(cdr(car l))))  'b)
(test  (car (cdr(cdr (car ( cdr l)))))   'f)




;; D) Usando solo cons y la lista vacía genere las siguientes expresiones: ' (a . b), ' (a
;;b), ' (a (b . c) d).

(test (cons 'a 'b) ' (a . b))
(test   (cons 'a (cons (cons 'b 'c) (cons 'd '())))         ' (a (b . c) d))




             