#lang scheme
(require "carta_199294229_CristopherAngulo.rkt")
(require "simbolo_199294229_CristopherAngulo.rkt")
(require "mazo_199294229_CristopherAngulo.rkt")
(require "jugador_199294229_CristopherAngulo.rkt")

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define elementsSet (list "A" "B" "C" "D" "E" "F" "Z" ))
(define symbols (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
(cardsSet symbols 3 7 3)


