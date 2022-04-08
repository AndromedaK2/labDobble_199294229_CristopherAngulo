#lang scheme
(require "carta_199294229_CristopherAngulo.rkt")
(require "simbolo_199294229_CristopherAngulo.rkt")
(require "mazo_199294229_CristopherAngulo.rkt")
(require "simbolo_199294229_CristopherAngulo.rkt")
(require "jugador_199294229_CristopherAngulo.rkt")

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define elementsSet (list "A" "B" "C" "D" "E" "F" "Z" ))
(cardsSet elementsSet 2 5 3)


