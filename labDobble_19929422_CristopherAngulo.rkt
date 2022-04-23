#lang scheme
(require "carta_19929422_CristopherAngulo.rkt")
(require "simbolo_19929422_CristopherAngulo.rkt")
(require "mazo_19929422_CristopherAngulo.rkt")
(require "jugador_19929422_CristopherAngulo.rkt")

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

;Elementos de ejemplo
;(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
;(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(define elements (list  1 2 3 4 5 6 7 8 9 10 11 12 13))
(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
;Mazo de carta de ejemplo
(cardsSet elementoss 3 -1 3)
(cardsSet elementoss 3 7 3)
(cardsSet elements   4 13 3)
(cardsSet elements   4 10 3)
;Cantidad de cartas del mazo
(numCards (cardsSet elementoss 3 7 3) )
(numCards (cardsSet elementoss 3 7 3) )
;Retornar Carta
(nthCard (cardsSet elements  4 13 3) 2 )
(nthCard (cardsSet elements   4 13 3) 2 )
;Encontrar total de cartas
(findTotalCards (nthCard (cardsSet elements 4 13 3) 2) )
(findTotalCards (nthCard (cardsSet elementoss 3 7 3) 2) )
;el juego es valido dobble
;(dobble? (cardsSet elements 4 13 3)) 
;(define elementoss (list (element "A") (element 2) (element "D") (element "C") (element "3") (element 8) (element 10)))
(dobble? (cardsSet elements 4 13 3)) 
(missingCards (cardsSet elementoss 3 5 3) elementoss) 

(display (cardsSet->string (cardsSet elements 4 13 3)))  



